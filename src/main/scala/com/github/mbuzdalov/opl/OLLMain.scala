package com.github.mbuzdalov.opl

object OLLMain {
  class Evaluator(n: Int, debugOutput: Boolean) {
    val lambdas: Array[Int] = Array.ofDim[Int](n + 1)
    val runtimes: Array[Double] = Array.ofDim[Double](n + 1)
    val totalRuntime: Double = {
      runtimes(n) = 0.0

      var theTotalRuntime = 0.0
      var x = n
      while (x > 0) {
        x -= 1
        // Choosing best discrete lambda
        var bestLambda = 0
        var bestValue = Double.PositiveInfinity
        var valueAt1 = Double.NaN
        var lambda = 1
        var worseInARow = 0
        while (lambda <= n) {
          val value = findRuntime(x, runtimes, lambda)
          if (value < bestValue) {
            bestValue = value
            bestLambda = lambda
          }
          if (value > bestValue) {
            worseInARow += 1
          }
          if (lambda == 1) {
            valueAt1 = value
          }
          if (worseInARow > 2) { // we hope for decent convexity of value as a function of lambda
            lambda = n // essentially, break
          }
          lambda += 1
        }
        runtimes(x) = bestValue
        lambdas(x) = bestLambda

        if (debugOutput) {
          println(s"$x => lambda=$bestLambda, value=$bestValue, value at 1=$valueAt1")
        }

        theTotalRuntime += bestValue * math.exp(MathEx.logChoose(n, x) - math.log(2) * n)
      }
      theTotalRuntime
    }


    private def findRuntime(x: Int, runtimes: Array[Double], lambda: Double): Double = {
      val popSize = math.round(lambda).toInt
      val mProb = lambda / n
      val xProb = 1 / lambda

      val xOver1X = xProb / (1 - xProb)

      val logMProb = math.log(mProb)
      val log1MProb = math.log1p(-mProb)

      var sumW, sumP = 0.0
      val probOfReachingF = Array.ofDim[Double](n + 1)

      // All that we do we condition on the distance between the parent and offspring.
      var d = 1
      while (d <= n) {
        // The maximum number of good bits to be flipped
        val maxG = math.min(d, n - x)
        var dProbability, dExpectation = 0.0
        var dCumulativeSum = if (x < d) 0.0 else math.exp(MathEx.logChoose(x, d) - MathEx.logChoose(n, d))

        // We need to go from the end: first model the crossover, then mutation atop of it.

        // In crossover, we evaluate the probability and expected remaining runtime
        // for each number of "good" bits flipped in the best selected offspring.
        // If the best offspring has g "good" bits flipped right, and hence d-g bits flipped wrong,
        // reaching fitness f from parent fitness x and "crossover probability" xProb
        // is possible when flipping f-x+i "good" bits and i "bad" bits for all possible i.
        // The probability is choose(g, f - x + i) * choose(d - g, i) * xProb^(f - x + 2 * i) * (1 - xProb)^(d - (f - x + 2 * i))
        var g = 1
        while (g <= maxG) {
          // In mutation, the offspring with most good bits flipped wins.
          // We know we flipped d bits, and there are x bad bits and n-x good bits.
          // In one run, the probability to flip g good bits is choose(n-x, g) * choose(x, d-g) / choose(n, d)
          // We may accumulate the probability in popSize runs just immediately, as well as we may collect the result.
          val pOfThisGInSingleMutation = if (x < d - g) 0.0 else math.exp(MathEx.logChoose(n - x, g) + MathEx.logChoose(x, d - g) - MathEx.logChoose(n, d))
          val newDCumulativeSum = dCumulativeSum + pOfThisGInSingleMutation
          val pOfThisGInAllMutations = if (popSize > 1) math.pow(newDCumulativeSum, popSize) - math.pow(dCumulativeSum, popSize) else pOfThisGInSingleMutation
          dCumulativeSum = newDCumulativeSum

          if (pOfThisGInAllMutations > 0 && (xProb < 1 || g > d - g)) {
            // Filling the probability of reaching a fitness (probOfReachingF(i) corresponds to fitness x + i)
            // in one application of crossover
            if (xProb == 1) {
              // Everything is flipped. This is a special quick case, as only one possible offspring is generated
              val theFitness = g - (d - g)
              dProbability += pOfThisGInAllMutations
              dExpectation += pOfThisGInAllMutations * runtimes(x + theFitness)
            } else {
              var goodFlip = 1
              var p0 = xProb * math.pow(1 - xProb, d - 1) * g
              while (goodFlip <= g) {
                probOfReachingF(goodFlip) = 0.0
                val badFlipLimit = math.min(goodFlip - 1, d - g)
                var badFlip = 0
                var p = p0
                while (badFlip <= badFlipLimit) {
                  probOfReachingF(goodFlip - badFlip) += p
                  p *= xOver1X * (d - g - badFlip)
                  badFlip += 1
                  p /= badFlip
                }
                p0 *= xOver1X * (g - goodFlip)
                goodFlip += 1
                p0 /= goodFlip
              }

              // The remaining probability is for being no better
              var probOfReachingFSum = 0.0

              {
                var i = 1
                while (i <= g) {
                  probOfReachingFSum += probOfReachingF(i)
                  i += 1
                }
              }
              probOfReachingF(0) = 1.0 - probOfReachingFSum
              if (probOfReachingF(0) < 0) {
                assert(probOfReachingF(0) >= -1e-9)
                probOfReachingF(0) = 0
              }

              // The above was for one application of crossover. Now it's time to use popSize
              if (popSize > 1) {
                var sum = 0.0
                probOfReachingFSum = 0
                var i = 0
                while (i <= g) {
                  val newSum = sum + probOfReachingF(i)
                  probOfReachingF(i) = math.pow(newSum, popSize) - math.pow(sum, popSize)
                  probOfReachingFSum += probOfReachingF(i)
                  sum = newSum
                  i += 1
                }

                assert(math.abs(1 - sum) < 1e-9, "Total probability is not 1")
                assert(math.abs(1 - probOfReachingFSum) < 1e-9, "Population sizing fails")
              }

              // Finally, compute the remaining time & probability, given g
              {
                var xProbOfImprovement, xRemainingTime = 0.0
                var i = 1
                while (i <= g) {
                  xProbOfImprovement += probOfReachingF(i)
                  xRemainingTime += probOfReachingF(i) * runtimes(x + i)
                  i += 1
                }
                dProbability += pOfThisGInAllMutations * xProbOfImprovement
                dExpectation += pOfThisGInAllMutations * xRemainingTime
              }
            }
          }

          g += 1
        }

        assert(math.abs(1 - dCumulativeSum) < 1e-9, "Total probability is not 1")

        // Finally, the probability to flip d bits in mutants is choose(n, d) * mProb^d * (1 - mProb)^(n-d)
        val multipleHere = if (mProb == 1)
          if (n == d) 1.0 else 0.0
        else
          math.exp(MathEx.logChoose(n, d) + d * logMProb + (n - d) * log1MProb)
        assert(dProbability.isFinite, s"Shit: dProbability = $dProbability")
        assert(multipleHere.isFinite, s"Shit: multipleHere = $multipleHere, d = $d, mProb = $mProb")
        sumP += dProbability * multipleHere
        sumW += dExpectation * multipleHere

        d += 1
      }

      assert(0 <= sumP && sumP <= 1 + 1e-9, s"Something is terribly wrong: sumP = $sumP, x = $x, lambda = $lambda")
      if (sumP > 1) {
        sumP = 1
      }

      // The final result is straightforward: we wait until success, then go the chosen way,
      // assuming we spend 2 * popSize in each iteration
      (sumW + 2 * popSize) / sumP
    }
  }


  def main(args: Array[String]): Unit = {
    val n = args(0).toInt
    val t0 = System.nanoTime()
    val evaluator = new Evaluator(n, debugOutput = true)
    println(s"Total runtime: ${evaluator.totalRuntime}")
    println(s"Time consumed: ${(System.nanoTime() - t0) * 1e-9} s")
  }
}
