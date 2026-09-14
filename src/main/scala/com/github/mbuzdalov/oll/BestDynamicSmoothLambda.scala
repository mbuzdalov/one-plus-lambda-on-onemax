package com.github.mbuzdalov.oll

import com.github.mbuzdalov.util.Loops.{loopFromDownTo, loopFromUntil}

import java.io.{FileOutputStream, PrintWriter}
import java.util.concurrent.ScheduledThreadPoolExecutor
import com.github.mbuzdalov.util.MathEx

object BestDynamicSmoothLambda:
  class Evaluator(ollComputation: OLLComputation, output: Option[String]):
    private val n = ollComputation.n
    private val lambdas: Array[Double] = Array.ofDim(n + 1)
    private val runtimes: Array[Double] = Array.ofDim(n + 1)

    val totalRuntime: Double =
      runtimes(n) = 0.0

      val nProcessors = Runtime.getRuntime.availableProcessors()
      val pool = ScheduledThreadPoolExecutor(nProcessors)
      val pw = output.map(name => PrintWriter(FileOutputStream(name), true))
      pw.foreach(ollComputation.logConfiguration)
      pw.foreach(_.println("fitness,best-lambda,runtime-to-optimum"))

      def runSmooth(x: Int, lambda: Double): Double =
        val lambdaDown = math.floor(lambda).toInt
        val lambdaUp = math.ceil(lambda).toInt
        if lambdaDown == lambdaUp then
          ollComputation.findRuntime(x, lambda, lambdaDown, runtimes).toDouble
        else
          val probDown = lambdaUp - lambda
          val probUp = lambda - lambdaDown
          val resultDown = ollComputation.findRuntime(x, lambda, lambdaDown, runtimes)
          val resultUp = ollComputation.findRuntime(x, lambda, lambdaUp, runtimes)
          (resultDown * probDown + resultUp * probUp).toDouble

      def runTernarySearch(x: Int, lambdaLo: Double, lambdaHi: Double): (Double, Double) =
        var left = lambdaLo
        var right = lambdaHi
        while right - left > 1e-9 * math.max(left, right) do
          val dissection = IndexedSeq.tabulate(nProcessors)(i => ((i + 1) * left + (nProcessors - i) * right) / (nProcessors + 1))
          val tasks = dissection.map(v => pool.submit(() => runSmooth(x, v)))
          val results = tasks.map(_.get())
          val bestValue = results.min
          val bestIndex = results.indexOf(bestValue)
          val center = dissection(bestIndex)
          val delta = (right - left) / (nProcessors + 1)
          left = center - delta
          right = center + delta
        end while
        val result = (left + right) / 2
        (result, runSmooth(x, result))

      val bounds = Seq(1.0, 1.5, n / 2.0, n.toDouble)

      loopFromDownTo(n - 1, 0): x =>
        var bestLambda = 1.0
        var bestValue = runSmooth(x, 1.0)

        loopFromUntil(1, bounds.size): i =>
          val (lambda, value) = runTernarySearch(x, bounds(i - 1), bounds(i))
          if bestValue > value then
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

    val crossoverComputation = CrossoverComputation.findMathCapableImplementation(cmd, "crossover-math")

    val ollComputation = OLLComputation(n,
      neverMutateZeroBits = cmd.getBoolean("never-mutate-zero-bits"),
      includeBestMutantInComparison = cmd.getBoolean("include-best-mutant"),
      ignoreCrossoverParentDuplicates = cmd.getBoolean("ignore-crossover-parent-duplicates"),
      crossoverComputation = crossoverComputation)

    val evaluator = Evaluator(ollComputation, output = cmd.getStringOption("output"))

    if printSummary then
      println(s"Total runtime: ${evaluator.totalRuntime}")
      println(s"Time consumed: ${(System.nanoTime() - t0) * 1e-9} s")
  end main
 