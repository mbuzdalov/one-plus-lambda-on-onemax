package com.github.mbuzdalov.oll

import java.{util => ju}
import java.util.concurrent.{Callable, ScheduledThreadPoolExecutor}

import scala.io.Source

import com.github.mbuzdalov.util.{MathEx, NumericMinimization}

object BestBinnedDoubleLambda {
  private def run(n: Int, bins: Seq[Int], lambdas: Array[Double], tlComputation: ThreadLocal[OLLComputation]): Double = {
    val ollComputation = tlComputation.get()
    val runtimes = new Array[Double](n + 1)
    for (i <- bins.size - 2 to 0 by -1) {
      val lambda: Double = lambdas(i)
      for (f <- bins(i + 1) - 1 to bins(i) by -1) {
        runtimes(f) = ollComputation.findRuntime(f, lambda, runtimes)
      }
    }
    ollComputation.crossoverComputation.clear()
    MathEx.expectedRuntimeOnBitStrings(n, runtimes)
  }

  private def optimize(n: Int, tlComputation: ThreadLocal[OLLComputation], bins: Seq[Int], pool: ScheduledThreadPoolExecutor)
                      (a: Array[NumericMinimization.CMAIndividual]): Unit = {
    val list = new ju.ArrayList[Callable[Unit]]
    a.foreach(ind => list.add(() => ind.setRawFitness(run(n, bins, ind.getFixedX, tlComputation))))
    pool.invokeAll(list)
  }

  def main(args: Array[String]): Unit = {
    val cmd = new CommandLineArgs(args)
    val input = Source.fromFile(cmd.getString("input", " (expected input filename)"))
    val lines = input.getLines().toIndexedSeq
    input.close()

    val (header, data) = lines.partition(_.startsWith("#"))
    val headerFiltered = header.map(_.substring(1).trim)
    val n = headerFiltered.head.substring("n=".length).toInt
    val cmd2 = new CommandLineArgs(headerFiltered.tail.toArray)

    val tlComputation = ThreadLocal.withInitial[OLLComputation](() => {
      val crossoverComputation = new InMemoryCostPrioritizingCrossoverCache(
        maxCacheByteSize = cmd.getLong("max-cache-byte-size"),
        delegate = CrossoverComputation,
        verbose = false)

      new OLLComputation(n,
        neverMutateZeroBits = cmd2.getBoolean("never-mutate-zero-bits"),
        includeBestMutantInComparison = cmd2.getBoolean("include-best-mutant"),
        ignoreCrossoverParentDuplicates = cmd2.getBoolean("ignore-crossover-parent-duplicates"),
        crossoverComputation = crossoverComputation)
    })

    val bins = (0 to 30).map(log => n - (n >>> log)).distinct.sorted
    val lambdaTable = data.drop(1).map(line => line.split(',')(1).toDouble).reverse
    val rawLambdaValues = new Array[Double](bins.length - 1)
    for (i <- rawLambdaValues.indices) {
      val sum = lambdaTable.indices.filter(j => bins(i) <= j && j < bins(i + 1)).map(lambdaTable).sum
      rawLambdaValues(i) = sum / (bins(i + 1) - bins(i))
    }
    println(s"Bins: ${bins.mkString(", ")}")
    rawLambdaValues(0) = 1

    val nonRoundedRuntime = run(n, bins, rawLambdaValues, tlComputation)
    println(s"Non-rounded runtime: $nonRoundedRuntime with ${rawLambdaValues.mkString(", ")}")

    val pool = new ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
    val (result, fitness) = NumericMinimization.optimizeDistributionBySeparableCMAES(
      initialMean = rawLambdaValues,
      lowerBound = _ => 1.0, upperBound = _ => n,
      function = optimize(n, tlComputation, bins, pool),
      initialSigma = cmd.getDouble("initial-sigma"),
      maxIterations = 100, populationSize = 100, nResamplingUntilFeasible = 10,
      logToConsole = true
    )
    pool.shutdown()

    println(s"Final result: ${result.mkString(", ")}")
    println(s"Final fitness: $fitness")
  }
}
