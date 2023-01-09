package com.github.mbuzdalov.oll

import java.util.concurrent.{Callable, ScheduledThreadPoolExecutor}
import java.{util => ju}

import scala.io.Source

import com.github.mbuzdalov.util.NumericMinimization

object BestBinnedDoubleLambda {
  private def lambdaToParts(lambda: Double, target: Array[Double], index: Int, n: Int): Unit = {
    val integral = math.round(lambda).toInt
    val fractional = lambda - integral
    assert(-0.5 <= fractional && fractional < 0.5)
    target(2 * index) = (integral - 1.0) / (n - 1)
    target(2 * index + 1) = fractional + 0.5
  }

  private def lambdaFromParts(source: Array[Double], index: Int, n: Int): Double = {
    val integral = source(2 * index)
    val fractional = source(2 * index + 1)
    math.max(1, (integral * (n - 1)).toInt + 1 + fractional - 0.5)
  }

  private def individualToLambdaString(ind: Array[Double], n: Int): String = {
    var i = 0
    val sb = new StringBuilder("[")
    while (i * 2 < ind.length) {
      if (i != 0) sb.append(", ")
      sb.append(lambdaFromParts(ind, i, n))
      i += 1
    }
    sb.result()
  }

  private def run(n: Int, bins: Seq[Int], lambdaGens: Array[Double], tlComputation: ThreadLocal[OLLComputation]): Double =
    RunGivenLambdas.run(n, bins, i => lambdaFromParts(lambdaGens, i, n), tlComputation.get())

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

    val bins = RunGivenLambdas.defaultBins(n)
    val lambdaTable = data.drop(1).map(line => line.split(',')(1).toDouble).reverse
    val rawLambdaValues = new Array[Double](2 * (bins.length - 1))
    for (i <- 0 until bins.length - 1) {
      val sum = lambdaTable.indices.filter(j => bins(i) <= j && j < bins(i + 1)).map(lambdaTable).sum
      val lambda = if (i == 0) 1 else sum / (bins(i + 1) - bins(i))
      lambdaToParts(lambda, rawLambdaValues, i, n)
    }
    println(s"Bins: ${bins.mkString(", ")}")

    val pool = new ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
    val (result, fitness) = NumericMinimization.optimizeDistributionBySeparableCMAES(
      initialMean = rawLambdaValues,
      lowerBound = _ => 0.0, upperBound = _ => 1.0,
      function = optimize(n, tlComputation, bins, pool),
      initialSigma = cmd.getDouble("initial-sigma"),
      maxIterations = 100, populationSize = 100, nResamplingUntilFeasible = 10,
      logToConsole = true
    )
    pool.shutdown()

    println(s"Final result: ${individualToLambdaString(result, n)}")
    println(s"Final fitness: $fitness")
  }
}
