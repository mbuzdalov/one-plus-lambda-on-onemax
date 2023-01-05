package com.github.mbuzdalov.oll

import java.io.{FileOutputStream, PrintWriter}
import java.util.concurrent.{Callable, ScheduledThreadPoolExecutor}

import com.github.mbuzdalov.math.MathEx

object BestDynamicIntegerLambda {
  class Evaluator(ollComputation: OLLComputation,
                  output: Option[String]) {

    private val n = ollComputation.n
    private val lambdas: Array[Int] = Array.ofDim[Int](n + 1)
    private val runtimes: Array[Double] = Array.ofDim[Double](n + 1)

    val totalRuntime: Double = {
      runtimes(n) = 0.0

      val pool = new ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
      val pw = output.map(name => new PrintWriter(new FileOutputStream(name), true))
      pw.foreach(ollComputation.logConfiguration)
      pw.foreach(_.println("fitness,best-lambda,runtime-to-optimum"))

      var x = n
      while (x > 0) {
        x -= 1
        // Choosing best discrete lambda
        var bestLambda = 0
        var bestValue = Double.PositiveInfinity

        val tasks = new java.util.ArrayList[Callable[Double]](n)
        for (lambda <- 1 to n) {
          tasks.add(() => ollComputation.findRuntime(x, lambda, runtimes))
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
      }

      pw.foreach(_.close())
      pool.shutdown()
      MathEx.expectedRuntimeOnBitStrings(n, runtimes)
    }
  }

  def main(args: Array[String]): Unit = {
    val n = args(0).toInt
    val cmd = new CommandLineArgs(args)
    val printSummary = cmd.getBoolean("print-summary")
    val t0 = System.nanoTime()

    val crossoverComputation = new InMemoryCostPrioritizingCrossoverCache(
      maxCacheByteSize = cmd.getLong("max-cache-byte-size"),
      delegate = CrossoverComputation,
      verbose = true)

    val ollComputation = new OLLComputation(n,
      neverMutateZeroBits = cmd.getBoolean("never-mutate-zero-bits"),
      includeBestMutantInComparison = cmd.getBoolean("include-best-mutant"),
      ignoreCrossoverParentDuplicates = cmd.getBoolean("ignore-crossover-parent-duplicates"),
      crossoverComputation = crossoverComputation)

    val evaluator = new Evaluator(ollComputation, output = cmd.getStringOption("output"))

    crossoverComputation.clear()
    if (printSummary) {
      println(s"Total runtime: ${evaluator.totalRuntime}")
      println(s"Time consumed: ${(System.nanoTime() - t0) * 1e-9} s")
    }
  }
}
