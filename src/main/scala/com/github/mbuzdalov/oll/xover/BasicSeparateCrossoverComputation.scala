package com.github.mbuzdalov.oll.xover

import com.github.mbuzdalov.oll.{AugmentedProbability, CrossoverComputation}
import com.github.mbuzdalov.util.MathEx

object BasicSeparateCrossoverComputation extends CrossoverComputation {
  override def compute(distanceToParent: Int, goodBitsInDifference: Int, populationSize: Int, crossoverBias: AugmentedProbability): Array[Double] = {
    val probOfReachingF = Array.ofDim[Double](goodBitsInDifference + 1)

    // For the Hamming distance between the parent and the offspring being `distanceToParent` = d,
    // the number of good bits in the offspring being `goodBitsInDifference` = g,
    // and hence d-g bits flipped wrong in the offspring,
    // reaching fitness f from parent fitness x using crossover bias `crossoverBias` = xProb
    // is possible when flipping f-x+i "good" bits and i "bad" bits for all possible i.
    // Denote y=f-x, the event has the probability choose(g, y+i) * choose(d-g, i) * xProb^(y+2*i) * (1-xProb)^(d-(y+2*i)).

    val badBitsInDifference = distanceToParent - goodBitsInDifference
    var sumP = 0.0
    var fitnessDiff = 1
    while (fitnessDiff <= goodBitsInDifference) {
      var localSum = 0.0
      val maxI = math.min(distanceToParent - goodBitsInDifference, goodBitsInDifference - fitnessDiff)
      var flippedBadBits = 0
      while (flippedBadBits <= maxI) {
        val flippedGoodBits = fitnessDiff + flippedBadBits
        val flippedBits = flippedBadBits + flippedGoodBits
        localSum += math.exp(MathEx.logChoose(goodBitsInDifference, flippedGoodBits)
          + MathEx.logChoose(badBitsInDifference, flippedBadBits)
          + crossoverBias.logarithm * flippedBits
          + crossoverBias.logarithmOfOneMinus * (distanceToParent - flippedBits))
        flippedBadBits += 1
      }
      probOfReachingF(fitnessDiff) = localSum
      sumP += localSum
      fitnessDiff += 1
    }

    if (sumP > 1) {
      assert(sumP <= 1 + 1e-9, s"sumP is too much: $sumP")
      sumP = 1
    }

    probOfReachingF(0) = 1 - sumP

    // Now we use populationSize to obtain the final result.
    // The basic idea is that we reach fitness f if all crossover offspring have fitness <= f,
    // but not of them have fitness <= f-1, which results in the infamous subtraction of powers.
    if (populationSize > 1) {
      var sum = 0.0
      var probOfReachingFSum = 0.0
      var i = 0
      while (i <= goodBitsInDifference) {
        val newSum = sum + probOfReachingF(i)
        probOfReachingF(i) = math.pow(newSum, populationSize) - math.pow(sum, populationSize)
        probOfReachingFSum += probOfReachingF(i)
        sum = newSum
        i += 1
      }

      assert(math.abs(1 - sum) < 1e-9, "Total probability is not 1")
      assert(math.abs(1 - probOfReachingFSum) < 1e-9, "Population sizing fails")
    }

    probOfReachingF
  }

  override def clear(): Unit = {}
}
