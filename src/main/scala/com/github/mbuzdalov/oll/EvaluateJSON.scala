package com.github.mbuzdalov.oll

import com.github.mbuzdalov.util.Loops.{loopFromDownTo, loopFromUntil}

import java.io.{BufferedReader, FileReader, PrintWriter}
import java.util.{StringTokenizer, ArrayList as JArrayList}
import java.util.concurrent.{Callable, ScheduledThreadPoolExecutor}
import scala.util.Using
import com.github.mbuzdalov.util.MathEx

object EvaluateJSON:
  def run(n: Int, lambdas: Int => Double, populationSizes: Int => Int, ollComputation: OLLComputation): Double = {
    val runtimes = Array.ofDim[Double](n + 1)
    loopFromDownTo(n - 1, 0): f =>
      val lambda = lambdas(f)
      val populationSize = populationSizes(f)
      runtimes(f) = ollComputation.findRuntime(f, lambda, populationSize, runtimes).toDouble
    MathEx.expectedRuntimeOnBitStrings(n, runtimes)
  }

  def main(args: Array[String]): Unit =
    val cmd = CommandLineArgs(args)
    val input = Using.resource(BufferedReader(FileReader(cmd.getString("input", "(expected input filename)"))))(_.readLine())

    Using.resource(PrintWriter(cmd.getString("output", "(expected output filename)"))): out =>
      val st = new StringTokenizer(input, ":[]{}, ")

      val nToken = "\"n\""
      val experimentToken = "\"experiment\""
      val empiricalRuntimeMeanToken = "\"empirical_runtime_mean\""
      val empiricalRuntimeStdToken = "\"empirical_runtime_std\""
      val lambdasToken = "\"lbds\""
      val computedRuntimeMeanToken = "\"computed_runtime_mean\""

      val tasks = new JArrayList[Callable[String]]
      while st.hasMoreTokens do
        assert(st.nextToken() == nToken)
        val n = st.nextToken().toInt
        assert(st.nextToken() == experimentToken)
        val experimentName = st.nextToken()
        assert(st.nextToken() == empiricalRuntimeMeanToken)
        val empiricalMean = st.nextToken().toDouble
        assert(st.nextToken() == empiricalRuntimeStdToken)
        val empiricalStd = st.nextToken().toDouble
        assert(st.nextToken() == lambdasToken)
        val lambdas = Array.fill(n)(st.nextToken().toDouble)

        tasks.add: () =>
          val crossoverComputation = InMemoryCostPrioritizingCrossoverCache(
            maxCacheByteSize = cmd.getLong("max-cache-byte-size"),
            delegate = CrossoverComputation.findMathCapableImplementation(cmd, "crossover-math"),
            verbose = false)

          val ollComputation = OLLComputation(n,
            neverMutateZeroBits = cmd.getBoolean("never-mutate-zero-bits"),
            includeBestMutantInComparison = cmd.getBoolean("include-best-mutant"),
            ignoreCrossoverParentDuplicates = cmd.getBoolean("ignore-crossover-parent-duplicates"),
            crossoverComputation = crossoverComputation)

          val result = run(n, lambdas, i => math.round(lambdas(i)).toInt, ollComputation)
          crossoverComputation.clear()
          println(s"$empiricalMean +- $empiricalStd => $result")

          s"{$nToken: $n, $experimentToken: $experimentName, $empiricalRuntimeMeanToken: $empiricalMean, $empiricalRuntimeStdToken: $empiricalStd, $computedRuntimeMeanToken: $result, $lambdasToken: ${lambdas.mkString("[", ", ", "]")}}"
      end while

      val pool = ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
      val results = pool.invokeAll(tasks)

      out.print("[")
      loopFromUntil(0, tasks.size): i =>
        if (i != 0) out.print(", ")
        out.print(results.get(i).get())
      out.println("]")

      pool.shutdown()
  end main
