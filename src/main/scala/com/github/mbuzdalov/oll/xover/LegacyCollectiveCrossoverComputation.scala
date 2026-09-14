package com.github.mbuzdalov.oll.xover

import com.github.mbuzdalov.oll.{AugmentedProbability, CrossoverComputation}
import com.github.mbuzdalov.util.Loops.loopFromTo
import com.github.mbuzdalov.util.MathEx

/**
 * This is the legacy way to compute crossover probabilities that does it in one particular funny order
 * and contains two cases, for small and not-so-small crossover biases.
 */
object LegacyCollectiveCrossoverComputation extends CrossoverComputation:
  override def compute(distanceToParent: Int, goodBitsInDifference: Int, populationSize: Int, 
                       crossoverBias: AugmentedProbability): Array[Double] =
    val probOfReachingF = Array.ofDim[Double](goodBitsInDifference + 1)

    // For the Hamming distance between the parent and the offspring being `distanceToParent` = d,
    // the number of good bits in the offspring being `goodBitsInDifference` = g,
    // and hence d-g bits flipped wrong in the offspring,
    // reaching fitness f from parent fitness x using crossover bias `crossoverBias` = xProb
    // is possible when flipping f-x+i "good" bits and i "bad" bits for all possible i.
    // That event has the probability choose(g, f-x+i) * choose(d-g, i) * xProb^(f-x+2*i) * (1-xProb)^(d-(f-x+2*i)).

    if crossoverBias.value < 0.5 then
      // It is convenient to rewrite two latter multiples into (1-xProb)^d and (xProb / (1-xProb))^(f-x+2*i),
      // so we precompute the ratio.
      val xOver1X = crossoverBias.pOverOneMinusP

      // Now we compute the probabilities of obtaining each fitness f (expressed as fitness delta f-x)
      // by ONE crossover application.
      val badBitsInDifference = distanceToParent - goodBitsInDifference
      var p0 = crossoverBias.value * math.pow(1 - crossoverBias.value, distanceToParent - 1) * goodBitsInDifference
      loopFromTo(1, goodBitsInDifference): goodFlip =>
        val badFlipLimit = math.min(goodFlip - 1, badBitsInDifference)
        var p = p0
        loopFromTo(0, badFlipLimit): badFlip =>
          probOfReachingF(goodFlip - badFlip) += p
          p *= xOver1X * (badBitsInDifference - badFlip)
          p /= (badFlip + 1)
        p0 *= xOver1X * (goodBitsInDifference - goodFlip)
        p0 /= (goodFlip + 1)
    else
      // It is convenient to rewrite two latter multiples into (1-xProb)^d and (xProb / (1-xProb))^(f-x+2*i),
      // so we precompute the ratio.
      val lCB = crossoverBias.logarithm
      val l1CB = crossoverBias.logarithmOfOneMinus
      val xOver1X = lCB - l1CB

      // Now we compute the probabilities of obtaining each fitness f (expressed as fitness delta f-x)
      // by ONE crossover application.
      val badBitsInDifference = distanceToParent - goodBitsInDifference
      var p0 = lCB + l1CB * (distanceToParent - 1) + MathEx.log(goodBitsInDifference)
      loopFromTo(1, goodBitsInDifference): goodFlip =>
        val badFlipLimit = math.min(goodFlip - 1, badBitsInDifference)
        var p = p0
        loopFromTo(0, badFlipLimit): badFlip =>
          probOfReachingF(goodFlip - badFlip) += math.exp(p)
          p += xOver1X + MathEx.log(badBitsInDifference - badFlip)
          p -= MathEx.log(badFlip + 1)
        p0 += xOver1X + MathEx.log(goodBitsInDifference - goodFlip)
        p0 -= MathEx.log(goodFlip + 1)
    end if
    
    // The remaining probability is for being no better
    var probOfReachingFSum = 0.0

    loopFromTo(1, goodBitsInDifference): i =>
      probOfReachingFSum += probOfReachingF(i)

    probOfReachingF(0) = 1.0 - probOfReachingFSum
    if probOfReachingF(0) < 0 then
      assert(probOfReachingF(0) >= -1e-9, s"${probOfReachingF(0)}")
      probOfReachingF(0) = 0

    // Now we use populationSize to obtain the final result.
    // The basic idea is that we reach fitness f if all crossover offspring have fitness <= f,
    // but not of them have fitness <= f-1, which results in the infamous subtraction of powers.
    if populationSize > 1 then
      var sum = 0.0
      probOfReachingFSum = 0
      loopFromTo(0, goodBitsInDifference): i =>
        val newSum = sum + probOfReachingF(i)
        probOfReachingF(i) = math.pow(newSum, populationSize) - math.pow(sum, populationSize)
        probOfReachingFSum += probOfReachingF(i)
        sum = newSum

      assert(math.abs(1 - sum) < 1e-9, "Total probability is not 1")
      assert(math.abs(1 - probOfReachingFSum) < 1e-9, "Population sizing fails")

    probOfReachingF

  /**
   * Cleans up all the resources used by the object. Does nothing in this stateless implementation.
   */
  override def clear(): Unit = ()
