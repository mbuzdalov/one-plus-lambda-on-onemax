package com.github.mbuzdalov.oll

import java.io.{BufferedReader, FileReader, PrintWriter}
import java.util.{StringTokenizer, ArrayList => JArrayList}
import java.util.concurrent.{Callable, ScheduledThreadPoolExecutor}

import scala.util.Using

import com.github.mbuzdalov.util.MathEx

object EvaluateJSON {
  def run(n: Int, lambdas: Int => Double, populationSizes: Int => Int, ollComputation: OLLComputation): Double = {
    val runtimes = new Array[Double](n + 1)
    for (f <- n - 1 to 0 by -1) {
      val lambda = lambdas(f)
      val populationSize = populationSizes(f)
      runtimes(f) = ollComputation.findRuntime(f, lambda, populationSize, runtimes)
    }
    MathEx.expectedRuntimeOnBitStrings(n, runtimes)
  }

  def main(args: Array[String]): Unit = {
    val cmd = new CommandLineArgs(args)
    val input = Using.resource(new BufferedReader(new FileReader(cmd.getString("input", "(expected input filename)"))))(_.readLine())

    Using.resource(new PrintWriter(cmd.getString("output", "(expected output filename)"))) { out =>
      val st = new StringTokenizer(input, ":[]{}, ")

      val nToken = "\"n\""
      val experimentToken = "\"experiment\""
      val empiricalRuntimeMeanToken = "\"empirical_runtime_mean\""
      val empiricalRuntimeStdToken = "\"empirical_runtime_std\""
      val lambdasToken = "\"lbds\""
      val computedRuntimeMeanToken = "\"computed_runtime_mean\""

      val tasks = new JArrayList[Callable[String]]
      while (st.hasMoreTokens) {
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

        tasks.add(() => {
          val crossoverComputation = new InMemoryCostPrioritizingCrossoverCache(
            maxCacheByteSize = cmd.getLong("max-cache-byte-size"),
            delegate = CrossoverComputation.findMathCapableImplementation(cmd, "crossover-math"),
            verbose = false)

          val ollComputation = new OLLComputation(n,
            neverMutateZeroBits = cmd.getBoolean("never-mutate-zero-bits"),
            includeBestMutantInComparison = cmd.getBoolean("include-best-mutant"),
            ignoreCrossoverParentDuplicates = cmd.getBoolean("ignore-crossover-parent-duplicates"),
            crossoverComputation = crossoverComputation)

          val result = run(n, lambdas, i => math.round(lambdas(i)).toInt, ollComputation)
          crossoverComputation.clear()
          println(s"$empiricalMean +- $empiricalStd => $result")

          s"{$nToken: $n, $experimentToken: $experimentName, $empiricalRuntimeMeanToken: $empiricalMean, $empiricalRuntimeStdToken: $empiricalStd, $computedRuntimeMeanToken: $result, $lambdasToken: ${lambdas.mkString("[", ", ", "]")}}"
        })
      }

      val pool = new ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
      val results = pool.invokeAll(tasks)

      out.print("[")
      for (i <- 0 until tasks.size) {
        if (i != 0) out.print(", ")
        out.print(results.get(i).get())
      }
      out.println("]")

      pool.shutdown()
    }
  }
}
