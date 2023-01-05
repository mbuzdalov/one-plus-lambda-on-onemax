package com.github.mbuzdalov.oll

import java.io.{FileOutputStream, PrintWriter}
import java.util.concurrent.{Callable, ScheduledThreadPoolExecutor}

import com.github.mbuzdalov.math.MathEx

object OLLMain {
  class Evaluator(n: Int,
                  neverMutateZeroBits: Boolean,
                  includeBestMutantInComparison: Boolean,
                  ignoreCrossoverParentDuplicates: Boolean,
                  maxCacheByteSize: Long,
                  output: Option[String]) {

    private val lambdas: Array[Int] = Array.ofDim[Int](n + 1)
    private val runtimes: Array[Double] = Array.ofDim[Double](n + 1)
    private val crossoverComputation = new InMemoryCostPrioritizingCrossoverCache(maxCacheByteSize, CrossoverComputation)

    val totalRuntime: Double = {
      runtimes(n) = 0.0

      val pool = new ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
      val pw = output.map(name => new PrintWriter(new FileOutputStream(name), true))
      pw.foreach(_.print(s"""# n=$n
                            |# --never-mutate-zero-bits=$neverMutateZeroBits
                            |# --include-best-mutant=$includeBestMutantInComparison
                            |# --ignore-crossover-parent-duplicates=$ignoreCrossoverParentDuplicates
                            |fitness,best-lambda,runtime-to-optimum
                            |""".stripMargin))

      var theTotalRuntime = 0.0
      var x = n
      while (x > 0) {
        x -= 1
        // Choosing best discrete lambda
        var bestLambda = 0
        var bestValue = Double.PositiveInfinity

        val tasks = new java.util.ArrayList[Callable[Double]](n)
        for (lambda <- 1 to n) {
          tasks.add(() => findRuntime(x, lambda))
        }
        val futures = pool.invokeAll(tasks)

        for (lambda <- 1 to n) {
          val value = futures.get(lambda - 1).get()
          if (value < bestValue) {
            bestValue = value
            bestLambda = lambda
          }
        }
        runtimes(x) = bestValue
        lambdas(x) = bestLambda

        pw.foreach(_.println(s"$x,$bestLambda,$bestValue"))
        theTotalRuntime += bestValue * math.exp(MathEx.logChoose(n, x) - math.log(2) * n)
      }

      pw.foreach(_.close())
      pool.shutdown()
      theTotalRuntime
    }

    crossoverComputation.clear()

    private def findRuntime(x: Int, lambda: Double): Double = {
      val popSize = math.round(lambda).toInt
      val mProb = lambda / n
      val xProb = 1 / lambda

      val logMProb = math.log(mProb)
      val log1MProb = math.log1p(-mProb)

      var sumW, sumP = 0.0
      var expectedPopSize = 0.0

      // neverMutateZeroBits #1: we divide the probability of flipping exactly d bits by 1-(1-mProb)^n.
      val mutationScale = if (neverMutateZeroBits)
        1 / (1 - math.exp(n * log1MProb))
      else 1

      // neverMutateZeroBits #2: the expected population size needs to take into account d = 0
      if (!neverMutateZeroBits) {
        val probability = math.exp(n * log1MProb)
        if (ignoreCrossoverParentDuplicates) {
          expectedPopSize += probability * popSize // all mutations happen, all crossovers ignored
        } else {
          expectedPopSize += probability * 2 * popSize // all mutations happen, all crossovers happen
        }
      }

      // All that we do we condition on the distance between the parent and offspring.
      var d = 1
      while (d <= n) {

        // The maximum number of good bits to be flipped
        val maxG = math.min(d, n - x)
        var dProbability, dExpectation = 0.0
        var dCumulativeSum = if (x < d) 0.0 else math.exp(MathEx.logChoose(x, d) - MathEx.logChoose(n, d))

        // We need to go from the end: first model the crossover, then mutation atop of it.
        // Everything about crossover, once d, g, popSize and xProb are fixed, is shared across fitness values
        // and can be precomputed (or cached), so we do it.

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
            if (xProb == 1) {
              // Everything is flipped, so we basically consider the best mutant.
              // This is a special quick case, as only one possible offspring is generated
              val theFitness = g - (d - g)
              dProbability += pOfThisGInAllMutations
              dExpectation += pOfThisGInAllMutations * runtimes(x + theFitness)
            } else {
              // Getting the probability of reaching a fitness (probOfReachingF(i) corresponds to fitness x + i)
              val probOfReachingF = crossoverComputation.compute(d, g, popSize, xProb)
              var xProbOfImprovement, xRemainingTime = 0.0
              // includeBestMutantInComparison: we compute the probabilities of getting all fitness values,
              // but for those smaller than the best mutant, which we know, we use the runtime value
              // corresponding to the best mutant
              val minimumFitnessToUse = if (includeBestMutantInComparison) g - (d - g) else -1
              var i = 1
              while (i <= g) {
                xProbOfImprovement += probOfReachingF(i)
                xRemainingTime += probOfReachingF(i) * runtimes(x + math.max(i, minimumFitnessToUse))
                i += 1
              }
              dProbability += pOfThisGInAllMutations * xProbOfImprovement
              dExpectation += pOfThisGInAllMutations * xRemainingTime
            }
          }

          g += 1
        }

        assert(math.abs(1 - dCumulativeSum) < 1e-9, "Total probability is not 1")

        // Finally, the probability to flip d bits in mutants is choose(n, d) * mProb^d * (1 - mProb)^(n-d)
        val multipleHere = if (mProb == 1)
          if (n == d) 1.0 else 0.0
        else
          math.exp(MathEx.logChoose(n, d) + d * logMProb + (n - d) * log1MProb) * mutationScale
        assert(dProbability.isFinite, s"dProbability = $dProbability")
        assert(multipleHere.isFinite, s"multipleHere = $multipleHere, d = $d, mProb = $mProb")
        sumP += dProbability * multipleHere
        sumW += dExpectation * multipleHere

        // ignoreCrossoverParentDuplicates: seems that influences only the expected iteration size
        val expectedIterationSize = if (ignoreCrossoverParentDuplicates) {
          popSize + popSize * (1 - math.pow(xProb, d) - math.pow(1 - xProb, d))
        } else 2.0 * popSize

        expectedPopSize += expectedIterationSize * multipleHere

        d += 1
      }

      assert(0 <= sumP && sumP <= 1 + 1e-5, s"Something is terribly wrong: sumP = $sumP, x = $x, lambda = $lambda")
      if (sumP > 1 + 1e-9) {
        println(s"Warning: sumP = $sumP")
      }
      if (sumP > 1) {
        sumP = 1
      }

      // The final result is straightforward: we wait until success, then go the chosen way,
      // assuming we spend `expectedPopSize` fitness evaluations in each iteration
      (sumW + expectedPopSize) / sumP
    }
  }

  private def getBooleanOption(args: Array[String], name: String): Boolean = {
    val prefix = "--" + name + "="
    args.find(_.startsWith(prefix)) match {
      case Some(arg) => arg.substring(prefix.length).toBoolean
      case None => throw new IllegalArgumentException(s"No option --$name is given (expected either ${prefix}true or ${prefix}false")
    }
  }

  //noinspection SameParameterValue
  private def getLongOption(args: Array[String], name: String): Long = {
    val prefix = "--" + name + "="
    args.find(_.startsWith(prefix)) match {
      case Some(arg) => arg.substring(prefix.length).toLongOption match {
        case Some(value) => value
        case None => throw new IllegalArgumentException(s"--$name is not followed by a valid 64-bit integer")
      }
      case None => throw new IllegalArgumentException(s"No option --$name is given (expected $prefix<number>")
    }
  }

  def main(args: Array[String]): Unit = {
    val n = args(0).toInt
    val printSummary = getBooleanOption(args, "print-summary")
    val t0 = System.nanoTime()
    val evaluator = new Evaluator(n,
      neverMutateZeroBits = getBooleanOption(args, "never-mutate-zero-bits"),
      includeBestMutantInComparison = getBooleanOption(args, "include-best-mutant"),
      ignoreCrossoverParentDuplicates = getBooleanOption(args, "ignore-crossover-parent-duplicates"),
      maxCacheByteSize = getLongOption(args, "max-cache-byte-size"),
      output = args.find(_.startsWith("--output=")).map(_.substring("--output=".length)))
    if (printSummary) {
      println(s"Total runtime: ${evaluator.totalRuntime}")
      println(s"Time consumed: ${(System.nanoTime() - t0) * 1e-9} s")
    }
  }
}
