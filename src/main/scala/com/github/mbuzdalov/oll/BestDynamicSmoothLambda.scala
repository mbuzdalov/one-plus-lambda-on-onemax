package com.github.mbuzdalov.oll

import java.io.{FileOutputStream, PrintWriter}
import java.util.concurrent.ScheduledThreadPoolExecutor

import com.github.mbuzdalov.util.MathEx

object BestDynamicSmoothLambda {
  class Evaluator(ollComputation: OLLComputation, output: Option[String]) {
    private val n = ollComputation.n
    private val lambdas: Array[Double] = Array.ofDim(n + 1)
    private val runtimes: Array[Double] = Array.ofDim(n + 1)

    val totalRuntime: Double = {
      runtimes(n) = 0.0

      val nProcessors = Runtime.getRuntime.availableProcessors()
      val pool = new ScheduledThreadPoolExecutor(nProcessors)
      val pw = output.map(name => new PrintWriter(new FileOutputStream(name), true))
      pw.foreach(ollComputation.logConfiguration)
      pw.foreach(_.println("fitness,best-lambda,runtime-to-optimum"))

      def runSmooth(x: Int, lambda: Double): Double = {
        val lambdaDown = math.floor(lambda).toInt
        val lambdaUp = math.ceil(lambda).toInt
        if (lambdaDown == lambdaUp) {
          ollComputation.findRuntime(x, lambda, lambdaDown, runtimes).toDouble
        } else {
          val probDown = lambdaUp - lambda
          val probUp = lambda - lambdaDown
          val resultDown = ollComputation.findRuntime(x, lambda, lambdaDown, runtimes)
          val resultUp = ollComputation.findRuntime(x, lambda, lambdaUp, runtimes)
          (resultDown * probDown + resultUp * probUp).toDouble
        }
      }

      def runTernarySearch(x: Int, lambdaLo: Double, lambdaHi: Double): (Double, Double) = {
        var left = lambdaLo
        var right = lambdaHi
        while (right - left > 1e-9 * math.max(left, right)) {
          val dissection = IndexedSeq.tabulate(nProcessors)(i => ((i + 1) * left + (nProcessors - i) * right) / (nProcessors + 1))
          val tasks = dissection.map(v => pool.submit(() => runSmooth(x, v)))
          val results = tasks.map(_.get())
          val bestValue = results.min
          val bestIndex = results.indexOf(bestValue)
          val center = dissection(bestIndex)
          val delta = (right - left) / (nProcessors + 1)
          left = center - delta
          right = center + delta
        }
        val result = (left + right) / 2
        (result, runSmooth(x, result))
      }

      val bounds = Seq(1.0, 1.5, n / 2.0, n)

      var x = n
      while (x > 0) {
        x -= 1

        var bestLambda = 1.0
        var bestValue = runSmooth(x, 1.0)

        for (i <- 1 until bounds.size) {
          val (lambda, value) = runTernarySearch(x, bounds(i - 1), bounds(i))
          if (bestValue > value) {
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

    val crossoverComputation = CrossoverComputation.findMathCapableImplementation(cmd, "crossover-math")

    val ollComputation = new OLLComputation(n,
      neverMutateZeroBits = cmd.getBoolean("never-mutate-zero-bits"),
      includeBestMutantInComparison = cmd.getBoolean("include-best-mutant"),
      ignoreCrossoverParentDuplicates = cmd.getBoolean("ignore-crossover-parent-duplicates"),
      crossoverComputation = crossoverComputation)

    val evaluator = new Evaluator(ollComputation, output = cmd.getStringOption("output"))

    if (printSummary) {
      println(s"Total runtime: ${evaluator.totalRuntime}")
      println(s"Time consumed: ${(System.nanoTime() - t0) * 1e-9} s")
    }
  }
}
