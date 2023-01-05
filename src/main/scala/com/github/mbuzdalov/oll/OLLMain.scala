package com.github.mbuzdalov.oll

import java.io.{FileOutputStream, PrintWriter}
import java.util.concurrent.{Callable, ScheduledThreadPoolExecutor}

import com.github.mbuzdalov.math.MathEx

object OLLMain {
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

      var theTotalRuntime = 0.0
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
        theTotalRuntime += bestValue * math.exp(MathEx.logChoose(n, x) - math.log(2) * n)
      }

      pw.foreach(_.close())
      pool.shutdown()
      theTotalRuntime
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

    val crossoverComputation = new InMemoryCostPrioritizingCrossoverCache(
      maxCacheByteSize = getLongOption(args, "max-cache-byte-size"),
      delegate = CrossoverComputation)

    val ollComputation = new OLLComputation(n,
      neverMutateZeroBits = getBooleanOption(args, "never-mutate-zero-bits"),
      includeBestMutantInComparison = getBooleanOption(args, "include-best-mutant"),
      ignoreCrossoverParentDuplicates = getBooleanOption(args, "ignore-crossover-parent-duplicates"),
      crossoverComputation = crossoverComputation)

    val evaluator = new Evaluator(ollComputation,
      output = args.find(_.startsWith("--output=")).map(_.substring("--output=".length)))

    crossoverComputation.clear()
    if (printSummary) {
      println(s"Total runtime: ${evaluator.totalRuntime}")
      println(s"Time consumed: ${(System.nanoTime() - t0) * 1e-9} s")
    }
  }
}
