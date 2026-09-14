package com.github.mbuzdalov.oll

import com.github.mbuzdalov.util.Loops.{loopFromDownTo, loopFromTo, loopFromUntil}

import java.io.{FileOutputStream, PrintWriter}
import java.util.concurrent.{Callable, ScheduledThreadPoolExecutor}
import com.github.mbuzdalov.util.MathEx

object BestDynamicDoubleLambda:
  class Evaluator(ollComputation: OLLComputation, nonCachingComputation: OLLComputation, output: Option[String]):
    private val n = ollComputation.n
    private val lambdas: Array[Double] = Array.ofDim(n + 1)
    private val runtimes: Array[Double] = Array.ofDim(n + 1)

    val totalRuntime: Double =
      runtimes(n) = 0.0

      val pool = ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
      val pw = output.map(name => PrintWriter(FileOutputStream(name), true))
      pw.foreach(ollComputation.logConfiguration)
      pw.foreach(_.println("fitness,best-lambda,runtime-to-optimum"))

      val eps = 1e-8

      loopFromDownTo(n - 1, 0): x =>
        var bestLambda = 0.0
        var bestValue = Double.PositiveInfinity

        val tasks = new java.util.ArrayList[Callable[Double]](n)

        def smallestLambda(popSize: Int): Double = math.max(1, popSize - 0.5)
        def largestLambda(popSize: Int): Double = math.min(n, math.nextDown(popSize + 0.5))

        loopFromTo(1, n): popSize =>
          val smallest = smallestLambda(popSize)
          val largest = largestLambda(popSize)
          tasks.add(() => ollComputation.findRuntime(parentFitness = x, lambda = smallest, populationSize = popSize, runtimes = runtimes).toDouble)
          tasks.add(() => ollComputation.findRuntime(parentFitness = x, lambda = smallest + eps, populationSize = popSize, runtimes = runtimes).toDouble)
          tasks.add(() => ollComputation.findRuntime(parentFitness = x, lambda = largest - eps, populationSize = popSize, runtimes = runtimes).toDouble)
          tasks.add(() => ollComputation.findRuntime(parentFitness = x, lambda = largest, populationSize = popSize, runtimes = runtimes).toDouble)

        val futures = pool.invokeAll(tasks)
        tasks.clear()

        loopFromTo(1, n): popSize =>
          val smallest = smallestLambda(popSize)
          val largest = largestLambda(popSize)
          val value0 = futures.get(4 * (popSize - 1) + 0).get()
          val value1 = futures.get(4 * (popSize - 1) + 1).get()
          val value2 = futures.get(4 * (popSize - 1) + 2).get()
          val value3 = futures.get(4 * (popSize - 1) + 3).get()

          if value1 < value0 * (1 - 1e-12) && value2 < value3 * (1 - 1e-12) then
            var left = smallest
            var right = largest
            var myBestLambda = -1.0
            var myBestValue = Double.PositiveInfinity
            val pieces = math.max(2, Runtime.getRuntime.availableProcessors())

            var iteration = 100
            var changed = true
            while iteration > 0 && changed do
              changed = false
              iteration -= 1
              loopFromUntil(0, pieces): t =>
                val thisLambda = (left * (pieces - t) + right * (t + 1)) / (pieces + 1)
                tasks.add(() => nonCachingComputation.findRuntime(parentFitness = x, lambda = thisLambda, populationSize = popSize, runtimes = runtimes).toDouble)
              val ternaryFutures = pool.invokeAll(tasks)
              tasks.clear()
              var smallestIdx = 0
              loopFromUntil(1, ternaryFutures.size()): t =>
                if ternaryFutures.get(t).get() < ternaryFutures.get(smallestIdx).get() 
                then smallestIdx = t
              val smallestValue = ternaryFutures.get(smallestIdx).get()
              val smallestLambda = (left * (pieces - smallestIdx) + right * (smallestIdx + 1)) / (pieces + 1)
              val newLeft = (left * (pieces - smallestIdx + 1) + right * smallestIdx) / (pieces + 1)
              val newRight = (left * (pieces - smallestIdx - 1) + right * (smallestIdx + 2)) / (pieces + 1)
              if smallestValue < myBestValue then
                myBestValue = smallestValue
                myBestLambda = smallestLambda
              changed = (left != newLeft) || (right != newRight)
              left = newLeft
              right = newRight
            end while
            
            println(s"[warning] x=$x, popSize=$popSize: special minimum needed! $value0, $value1, $value2, $value3. Lambdas are $smallest and $largest. $myBestLambda => $myBestValue")
            if !(myBestValue <= value0 && myBestValue <= value1 && myBestValue <= value2 && myBestValue <= value3) then
              println(s"    [ERROR] $myBestValue exceeds one of [$value0, $value1, $value2, $value3]")
            if myBestValue < bestValue then
              bestValue = myBestValue
              bestLambda = myBestLambda
          else
            if value0 < value3 * (1 - 1e-12) && value1 < value0 * (1 - 1e-12) || value3 < value0 * (1 - 1e-12) && value2 < value3 * (1 - 1e-12) then
              println(s"[ERROR] x=$x, popSize=$popSize: Some serious non-monotone shit happens! $value0, $value1, $value2, $value3")
            if value0 < bestValue then
              bestValue = value0
              bestLambda = smallest
            if value3 < bestValue then
              bestValue = value3
              bestLambda = largest
          end if
        
        runtimes(x) = bestValue
        lambdas(x) = bestLambda

        pw.foreach(_.println(s"$x,$bestLambda,$bestValue"))

      pw.foreach(_.close())
      pool.shutdown()
      MathEx.expectedRuntimeOnBitStrings(n, runtimes)
    end totalRuntime
  end Evaluator

  def main(args: Array[String]): Unit =
    val n = args(0).toInt
    val cmd = CommandLineArgs(args)
    val printSummary = cmd.getBoolean("print-summary")
    val t0 = System.nanoTime()

    val nonCachingCrossoverComputation = CrossoverComputation.findMathCapableImplementation(cmd, "crossover-math")
    val crossoverComputation = InMemoryCostPrioritizingCrossoverCache(
      maxCacheByteSize = cmd.getLong("max-cache-byte-size"),
      delegate = nonCachingCrossoverComputation,
      verbose = true)

    val nonCachingOLLComputation = OLLComputation(n,
      neverMutateZeroBits = cmd.getBoolean("never-mutate-zero-bits"),
      includeBestMutantInComparison = cmd.getBoolean("include-best-mutant"),
      ignoreCrossoverParentDuplicates = cmd.getBoolean("ignore-crossover-parent-duplicates"),
      crossoverComputation = nonCachingCrossoverComputation)

    val ollComputation = OLLComputation(n,
      neverMutateZeroBits = cmd.getBoolean("never-mutate-zero-bits"),
      includeBestMutantInComparison = cmd.getBoolean("include-best-mutant"),
      ignoreCrossoverParentDuplicates = cmd.getBoolean("ignore-crossover-parent-duplicates"),
      crossoverComputation = crossoverComputation)

    val evaluator = Evaluator(ollComputation, nonCachingOLLComputation, output = cmd.getStringOption("output"))

    crossoverComputation.clear()
    if printSummary then
      println(s"Total runtime: ${evaluator.totalRuntime}")
      println(s"Time consumed: ${(System.nanoTime() - t0) * 1e-9} s")
  end main
