package com.github.mbuzdalov.oll

import java.io.{FileOutputStream, PrintWriter}
import java.util.concurrent.{Callable, Future, ScheduledThreadPoolExecutor}

import com.github.mbuzdalov.util.MathEx

object BestDynamicDoubleLambdaCheating {
  class Evaluator(ollComputation: OLLComputation, nonCachingComputation: OLLComputation, output: Option[String]) {
    private val n = ollComputation.n
    private val lambdas: Array[Double] = Array.ofDim(n + 1)
    private val runtimes: Array[Double] = Array.ofDim(n + 1)

    val totalRuntime: Double = {
      runtimes(n) = 0.0

      val pool, pool2 = new ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
      val pw = output.map(name => new PrintWriter(new FileOutputStream(name), true))
      pw.foreach(ollComputation.logConfiguration)
      pw.foreach(_.println("fitness,best-lambda,runtime-to-optimum"))

      val eps = 1e-8

      var x = n
      while (x > 0) {
        x -= 1
        var bestLambda = 0.0
        var bestValue = Double.PositiveInfinity

        def smallestLambda(popSize: Int): Double = math.max(1, popSize - 0.5)
        def largestLambda(popSize: Int): Double = math.min(n, math.nextDown(popSize + 0.5))

        def schedulePopSize(popSize: Int): Future[(Double, Double)] = pool2.submit(() => {
          val smallest = smallestLambda(popSize)
          val largest = largestLambda(popSize)
          val task0 = pool.submit(() => ollComputation.findRuntime(parentFitness = x, lambda = smallest, populationSize = popSize, runtimes = runtimes).toDouble)
          val task1 = pool.submit(() => ollComputation.findRuntime(parentFitness = x, lambda = smallest + eps, populationSize = popSize, runtimes = runtimes).toDouble)
          val task2 = pool.submit(() => ollComputation.findRuntime(parentFitness = x, lambda = largest - eps, populationSize = popSize, runtimes = runtimes).toDouble)
          val task3 = pool.submit(() => ollComputation.findRuntime(parentFitness = x, lambda = largest, populationSize = popSize, runtimes = runtimes).toDouble)
          val value0 = task0.get()
          val value1 = task1.get()
          val value2 = task2.get()
          val value3 = task3.get()

          if (value1 < value0 * (1 - 1e-12) && value2 < value3 * (1 - 1e-12)) {
            var left = smallest
            var right = largest
            var myBestLambda = -1.0
            var myBestValue = Double.PositiveInfinity
            val pieces = math.max(2, Runtime.getRuntime.availableProcessors())
            var iteration = 100
            var changed = true
            while (iteration > 0 && changed) {
              changed = false
              iteration -= 1
              val tasks = new java.util.ArrayList[Callable[Double]](n)
              for (t <- 0 until pieces) {
                val thisLambda = (left * (pieces - t) + right * (t + 1)) / (pieces + 1)
                tasks.add(() => nonCachingComputation.findRuntime(parentFitness = x, lambda = thisLambda, populationSize = popSize, runtimes = runtimes).toDouble)
              }
              val ternaryFutures = pool.invokeAll(tasks)
              tasks.clear()
              var smallestIdx = 0
              for (t <- 1 until ternaryFutures.size()) {
                if (ternaryFutures.get(t).get() < ternaryFutures.get(smallestIdx).get()) {
                  smallestIdx = t
                }
              }
              val smallestValue = ternaryFutures.get(smallestIdx).get()
              val smallestLambda = (left * (pieces - smallestIdx) + right * (smallestIdx + 1)) / (pieces + 1)
              val newLeft = (left * (pieces - smallestIdx + 1) + right * smallestIdx) / (pieces + 1)
              val newRight = (left * (pieces - smallestIdx - 1) + right * (smallestIdx + 2)) / (pieces + 1)
              if (smallestValue < myBestValue) {
                myBestValue = smallestValue
                myBestLambda = smallestLambda
              }
              changed = (left != newLeft) || (right != newRight)
              left = newLeft
              right = newRight
            }
            println(s"[warning] x=$x, popSize=$popSize: special minimum needed! $value0, $value1, $value2, $value3. Lambdas are $smallest and $largest. $myBestLambda => $myBestValue")
            if (!(myBestValue <= value0 && myBestValue <= value1 && myBestValue <= value2 && myBestValue <= value3)) {
              println(s"    [ERROR] $myBestValue exceeds one of [$value0, $value1, $value2, $value3]")
            }

            (myBestLambda, myBestValue)
          } else {
            if (value0 < value3 * (1 - 1e-12) && value1 < value0 * (1 - 1e-12) || value3 < value0 * (1 - 1e-12) && value2 < value3 * (1 - 1e-12)) {
              println(s"[ERROR] x=$x, popSize=$popSize: Some serious non-monotone shit happens! $value0, $value1, $value2, $value3")
            }
            if (value0 < value3) {
              (smallest, value0)
            } else {
              (largest, value3)
            }
          }
        })

        val testPopSizes = Seq(1, 2, 3, 4, n - n / 2, n - n / 4, n - n / 8, n)
        val testResults = testPopSizes.map(schedulePopSize).map(_.get())

        for ((lambda, value) <- testResults) {
          if (bestValue > value) {
            bestValue = value
            bestLambda = lambda
          }
        }

        if (testResults(2)._2 > testResults(3)._2) {
          // Find a small lambda that is better
          var prevValue = testResults(3)._2
          var popSize = 5
          var (currLambda, currValue) = schedulePopSize(popSize).get()
          while (popSize < n && prevValue > currValue) {
            if (bestValue > currValue) {
              bestValue = currValue
              bestLambda = currLambda
            }
            popSize += 1
            val (nextLambda, nextValue) = schedulePopSize(popSize).get()
            prevValue = currValue
            currLambda = nextLambda
            currValue = nextValue
          }
        }

        if (testResults(4)._2 > testResults(5)._2 || testResults(5)._2 > testResults(6)._2 || testResults(6)._2 > testResults(7)._2) {
          // Find a large lambda that is better
          var prevValue = testResults(7)._2
          var popSize = n - 1
          var (currLambda, currValue) = schedulePopSize(popSize).get()
          while (popSize > 1 && prevValue > currValue) {
            if (bestValue > currValue) {
              bestValue = currValue
              bestLambda = currLambda
            }
            popSize -= 1
            val (nextLambda, nextValue) = schedulePopSize(popSize).get()
            prevValue = currValue
            currLambda = nextLambda
            currValue = nextValue
          }
        }

        runtimes(x) = bestValue
        lambdas(x) = bestLambda

        pw.foreach(_.println(s"$x,$bestLambda,$bestValue"))
      }

      pw.foreach(_.close())
      pool.shutdown()
      pool2.shutdown()
      MathEx.expectedRuntimeOnBitStrings(n, runtimes)
    }
  }

  def main(args: Array[String]): Unit = {
    val n = args(0).toInt
    val cmd = new CommandLineArgs(args)
    val printSummary = cmd.getBoolean("print-summary")
    val t0 = System.nanoTime()

    val nonCachingCrossoverComputation = CrossoverComputation.findMathCapableImplementation(cmd, "crossover-math")
    val crossoverComputation = new InMemoryCostPrioritizingCrossoverCache(
      maxCacheByteSize = cmd.getLong("max-cache-byte-size"),
      delegate = nonCachingCrossoverComputation,
      verbose = true)

    val nonCachingOLLComputation = new OLLComputation(n,
      neverMutateZeroBits = cmd.getBoolean("never-mutate-zero-bits"),
      includeBestMutantInComparison = cmd.getBoolean("include-best-mutant"),
      ignoreCrossoverParentDuplicates = cmd.getBoolean("ignore-crossover-parent-duplicates"),
      crossoverComputation = nonCachingCrossoverComputation)

    val ollComputation = new OLLComputation(n,
      neverMutateZeroBits = cmd.getBoolean("never-mutate-zero-bits"),
      includeBestMutantInComparison = cmd.getBoolean("include-best-mutant"),
      ignoreCrossoverParentDuplicates = cmd.getBoolean("ignore-crossover-parent-duplicates"),
      crossoverComputation = crossoverComputation)

    val evaluator = new Evaluator(ollComputation, nonCachingOLLComputation, output = cmd.getStringOption("output"))

    crossoverComputation.clear()
    if (printSummary) {
      println(s"Total runtime: ${evaluator.totalRuntime}")
      println(s"Time consumed: ${(System.nanoTime() - t0) * 1e-9} s")
    }
  }
}
