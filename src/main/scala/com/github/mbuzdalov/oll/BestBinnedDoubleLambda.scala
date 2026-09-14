package com.github.mbuzdalov.oll

import com.github.mbuzdalov.util.Loops.loopFromUntil

import java.util.concurrent.{Callable, ScheduledThreadPoolExecutor}
import java.util as ju
import scala.io.Source
import com.github.mbuzdalov.util.NumericMinimization

object BestBinnedDoubleLambda:
  private def lambdaToParts(lambda: Double, target: Array[Double], index: Int, n: Int): Unit =
    val integral = math.round(lambda).toInt
    val fractional = lambda - integral
    assert(-0.5 <= fractional && fractional < 0.5)
    target(2 * index) = (integral - 1.0) / (n - 1)
    target(2 * index + 1) = fractional + 0.5

  private def lambdaFromParts(source: Array[Double], index: Int, n: Int): Double =
    val integral = source(2 * index)
    val fractional = source(2 * index + 1)
    math.max(1, (integral * (n - 1)).toInt + 1 + fractional - 0.5)

  private def individualToLambdaString(ind: Array[Double], n: Int): String =
    val sb = StringBuilder("[")
    loopFromUntil(0, ind.length / 2): i =>
      if i != 0 then sb.append(", ")
      sb.append(lambdaFromParts(ind, i, n))
    sb.result()

  private def run(n: Int, bins: Seq[Int], lambdaGens: Array[Double], tlComputation: ThreadLocal[OLLComputation]): Double =
    RunGivenLambdas.run(
      n = n,
      bins = bins,
      lambdas = i => lambdaFromParts(lambdaGens, i, n),
      populationSizes = i => math.round(lambdaFromParts(lambdaGens, i, n)).toInt,
      ollComputation = tlComputation.get())

  private def optimize(n: Int, tlComputation: ThreadLocal[OLLComputation], bins: Seq[Int], pool: ScheduledThreadPoolExecutor)
                      (a: Array[NumericMinimization.CMAIndividual]): Unit =
    val list = ju.ArrayList[Callable[Unit]]()
    a.foreach(ind => list.add(() => ind.setRawFitness(run(n, bins, ind.getFixedX, tlComputation))))
    pool.invokeAll(list)

  def main(args: Array[String]): Unit =
    val cmd = CommandLineArgs(args)
    val input = Source.fromFile(cmd.getString("input", " (expected input filename)"))
    val lines = input.getLines().toIndexedSeq
    input.close()

    val (header, data) = lines.partition(_.startsWith("#"))
    val headerFiltered = header.map(_.substring(1).trim)
    val n = headerFiltered.head.substring("n=".length).toInt
    val cmd2 = CommandLineArgs(headerFiltered.tail.toArray)

    val tlComputation = ThreadLocal.withInitial[OLLComputation](() => {
      val crossoverComputation = InMemoryCostPrioritizingCrossoverCache(
        maxCacheByteSize = cmd.getLong("max-cache-byte-size"),
        delegate = CrossoverComputation.findMathCapableImplementation(cmd, "crossover-math"),
        verbose = false)

      OLLComputation(n,
        neverMutateZeroBits = cmd2.getBoolean("never-mutate-zero-bits"),
        includeBestMutantInComparison = cmd2.getBoolean("include-best-mutant"),
        ignoreCrossoverParentDuplicates = cmd2.getBoolean("ignore-crossover-parent-duplicates"),
        crossoverComputation = crossoverComputation)
    })

    val bins = RunGivenLambdas.defaultBins(n)
    println(s"Bins: ${bins.mkString(", ")}")
    val rawLambdaValues = Array.ofDim[Double](2 * (bins.length - 1))

    cmd.getStringOption("initial-value") match
      case Some(initialValueStr) =>
        val tokenizer = ju.StringTokenizer(initialValueStr, ",")
        if tokenizer.countTokens() != bins.length - 1 then
          throw IllegalArgumentException(s"Number of tokens in initial-value should equal ${bins.length - 1}")
        loopFromUntil(0, bins.length - 1): i =>  
          val lambda = tokenizer.nextToken().toDouble
          lambdaToParts(lambda, rawLambdaValues, i, n)
      case None =>
        val lambdaTable = data.drop(1).map(line => line.split(',')(1).toDouble).reverse
        loopFromUntil(0, bins.length - 1): i =>
          val sum = lambdaTable.indices.filter(j => bins(i) <= j && j < bins(i + 1)).map(lambdaTable).sum
          val lambda = if i == 0 then 1 else sum / (bins(i + 1) - bins(i))
          lambdaToParts(lambda, rawLambdaValues, i, n)

    val pool = ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
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
  end main
