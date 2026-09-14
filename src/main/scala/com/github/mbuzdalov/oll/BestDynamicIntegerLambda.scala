package com.github.mbuzdalov.oll

import com.github.mbuzdalov.util.Loops.{loopFromDownTo, loopFromTo}

import java.io.{FileOutputStream, PrintWriter}
import java.util.concurrent.{Callable, ScheduledThreadPoolExecutor}
import com.github.mbuzdalov.util.MathEx

object BestDynamicIntegerLambda:
  class Evaluator(ollComputation: OLLComputation, output: Option[String]):
    private val n = ollComputation.n
    private val lambdas: Array[Int] = Array.ofDim[Int](n + 1)
    private val runtimes: Array[Double] = Array.ofDim[Double](n + 1)

    val totalRuntime: Double =
      runtimes(n) = 0.0

      val pool = ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
      val pw = output.map(name => PrintWriter(FileOutputStream(name), true))
      pw.foreach(ollComputation.logConfiguration)
      pw.foreach(_.println("fitness,best-lambda,runtime-to-optimum"))

      loopFromDownTo(n - 1, 0): x =>
        // Choosing best discrete lambda
        var bestLambda = 0
        var bestValue = Double.PositiveInfinity

        val tasks = java.util.ArrayList[Callable[Double]](n)
        loopFromTo(1, n): lambda =>
          tasks.add(() => ollComputation.findRuntime(parentFitness = x, lambda = lambda,
            populationSize = lambda, runtimes = runtimes).toDouble)
        val futures = pool.invokeAll(tasks)

        loopFromTo(1, n): lambda =>
          val value = futures.get(lambda - 1).get()
          if value < bestValue then 
            bestValue = value
            bestLambda = lambda
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

    val crossoverComputation = InMemoryCostPrioritizingCrossoverCache(
      maxCacheByteSize = cmd.getLong("max-cache-byte-size"),
      delegate = CrossoverComputation.findMathCapableImplementation(cmd, "crossover-math"),
      verbose = true)

    val ollComputation = OLLComputation(n,
      neverMutateZeroBits = cmd.getBoolean("never-mutate-zero-bits"),
      includeBestMutantInComparison = cmd.getBoolean("include-best-mutant"),
      ignoreCrossoverParentDuplicates = cmd.getBoolean("ignore-crossover-parent-duplicates"),
      crossoverComputation = crossoverComputation)

    val evaluator = Evaluator(ollComputation, output = cmd.getStringOption("output"))

    crossoverComputation.clear()
    if printSummary then
      println(s"Total runtime: ${evaluator.totalRuntime}")
      println(s"Time consumed: ${(System.nanoTime() - t0) * 1e-9} s")
  end main
