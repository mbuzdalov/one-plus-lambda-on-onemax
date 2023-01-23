package com.github.mbuzdalov.oll.xover

import com.github.mbuzdalov.oll.{AugmentedProbability, CrossoverComputation}
import com.github.mbuzdalov.util.MathEx

object ZeroCheckingSeparateCrossoverComputation extends CrossoverComputation {
  private val singularArray = Array.fill(1)(1.0)
  private val eps = 1e-20

  override def compute(distanceToParent: Int, goodBitsInDifference: Int, populationSize: Int, crossoverBias: AugmentedProbability): Array[Double] = {
    var probOfReachingF: Array[Double] = null

    // For the Hamming distance between the parent and the offspring being `distanceToParent` = d,
    // the number of good bits in the offspring being `goodBitsInDifference` = g,
    // and hence d-g bits flipped wrong in the offspring,
    // reaching fitness f from parent fitness x using crossover bias `crossoverBias` = xProb
    // is possible when flipping f-x+i "good" bits and i "bad" bits for all possible i.
    // Denote y=f-x, the event has the probability choose(g, y+i) * choose(d-g, i) * xProb^(y+2*i) * (1-xProb)^(d-(y+2*i)).

    var sumP = 0.0
    var fitnessDiff = goodBitsInDifference
    while (fitnessDiff >= 1) {
      val maxI = math.min(distanceToParent - goodBitsInDifference, goodBitsInDifference - fitnessDiff)
      var localSum = 0.0
      if (maxI == 0) {
        localSum += theExpr(distanceToParent, goodBitsInDifference, fitnessDiff, 0, crossoverBias)
      } else {
        // Now we are trying to guess the index such that the value at it is the maximum
        val qq = crossoverBias.pOverOneMinusP * crossoverBias.pOverOneMinusP
        val q2 = qq - 1
        val q1 = qq * (fitnessDiff - distanceToParent) - (fitnessDiff + 2)
        val q0 = qq * (fitnessDiff - goodBitsInDifference) * (goodBitsInDifference - distanceToParent) - (fitnessDiff + 1)
        val testBestI = if (math.abs(q2) < 1e-9) {
          // The linear case
          (-q0 / q1).toInt
        } else {
          // The quadratic case
          ((-q1 - math.sqrt(q1 * q1 - 4 * q0 * q2)) / (2 * q2)).toInt
        }
        val bestI = math.min(maxI, math.max(0, testBestI))

        localSum = theExpr(distanceToParent, goodBitsInDifference, fitnessDiff, bestI, crossoverBias)
        if (localSum * (maxI + 1) < eps) {
          // Do nothing, as the result will not exceed `eps` by monotonicity, and hence will be useless
        } else {
          var flippedBadBits = bestI - 1
          while (flippedBadBits >= 0) {
            val oldLocalSum = localSum
            localSum += theExpr(distanceToParent, goodBitsInDifference, fitnessDiff, flippedBadBits, crossoverBias)
            if (oldLocalSum == localSum) {
              flippedBadBits = 0
            }
            flippedBadBits -= 1
          }
          flippedBadBits = bestI + 1
          while (flippedBadBits <= maxI) {
            val oldLocalSum = localSum
            localSum += theExpr(distanceToParent, goodBitsInDifference, fitnessDiff, flippedBadBits, crossoverBias)
            if (oldLocalSum == localSum) {
              flippedBadBits = maxI
            }
            flippedBadBits += 1
          }
        }
      }

      if (localSum > eps) {
        if (probOfReachingF == null) {
          probOfReachingF = new Array[Double](fitnessDiff + 1)
        }
        probOfReachingF(fitnessDiff) = localSum
      }

      sumP += localSum
      fitnessDiff -= 1
    }

    if (sumP > 1) {
      assert(sumP <= 1 + 1e-9, s"sumP is too much: $sumP")
      sumP = 1
    }

    if (probOfReachingF == null) {
      singularArray
    } else {
      probOfReachingF(0) = 1 - sumP

      // Now we use populationSize to obtain the final result.
      // The basic idea is that we reach fitness f if all crossover offspring have fitness <= f,
      // but not of them have fitness <= f-1, which results in the infamous subtraction of powers.
      if (populationSize > 1) {
        var sum = 0.0
        var probOfReachingFSum = 0.0
        var i = 0
        while (i < probOfReachingF.length) {
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
  }

  private def theExpr(distanceToParent: Int, goodBitsInDifference: Int,
                      fitnessDiff: Int, flippedBadBits: Int, crossoverBias: AugmentedProbability): Double = {
    val flippedGoodBits = fitnessDiff + flippedBadBits
    val flippedBits = flippedBadBits + flippedGoodBits
    math.exp(MathEx.logChoose(goodBitsInDifference, flippedGoodBits)
      + MathEx.logChoose(distanceToParent - goodBitsInDifference, flippedBadBits)
      + crossoverBias.logarithm * flippedBits
      + crossoverBias.logarithmOfOneMinus * (distanceToParent - flippedBits))
  }

  override def clear(): Unit = {}
}
