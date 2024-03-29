package com.github.mbuzdalov.oll

import java.util.Random

import scala.io.Source

object BestBinnedIntegerLambda {
  def main(args: Array[String]): Unit = {
    val cmd = new CommandLineArgs(args)
    val input = Source.fromFile(cmd.getString("input", " (expected input filename)"))
    val lines = input.getLines().toIndexedSeq
    input.close()

    val (header, data) = lines.partition(_.startsWith("#"))
    val headerFiltered = header.map(_.substring(1).trim)
    val n = headerFiltered.head.substring("n=".length).toInt
    val cmd2 = new CommandLineArgs(headerFiltered.tail.toArray)

    val crossoverComputation = new InMemoryCostPrioritizingCrossoverCache(
      maxCacheByteSize = cmd.getLong("max-cache-byte-size"),
      delegate = CrossoverComputation.findMathCapableImplementation(cmd, "crossover-math"),
      verbose = false)

    val ollComputation = new OLLComputation(n,
      neverMutateZeroBits = cmd2.getBoolean("never-mutate-zero-bits"),
      includeBestMutantInComparison = cmd2.getBoolean("include-best-mutant"),
      ignoreCrossoverParentDuplicates = cmd2.getBoolean("ignore-crossover-parent-duplicates"),
      crossoverComputation = crossoverComputation)

    val bins = RunGivenLambdas.defaultBins(n)
    val lambdaTable = data.drop(1).map(line => line.split(',')(1).toDouble).reverse
    val rawLambdaValues = new Array[Double](bins.length - 1)
    for (i <- rawLambdaValues.indices) {
      val sum = lambdaTable.indices.filter(j => bins(i) <= j && j < bins(i + 1)).map(lambdaTable).sum
      rawLambdaValues(i) = sum / (bins(i + 1) - bins(i))
    }
    println(s"Bins: ${bins.mkString(", ")}")
    rawLambdaValues(0) = 1

    val nonRoundedRuntime = RunGivenLambdas.run(n, bins, rawLambdaValues, i => math.round(rawLambdaValues(i)).toInt, ollComputation)
    println(s"Non-rounded runtime: $nonRoundedRuntime with ${rawLambdaValues.mkString(", ")}")

    val lambdaValues = rawLambdaValues.map(v => math.round(v).toInt)

    var currentRuntime = RunGivenLambdas.run(n, bins, i => lambdaValues(i), lambdaValues, ollComputation)
    println(s"Initial runtime: $currentRuntime with ${lambdaValues.mkString(", ")}")
    var changed = false
    val rng = new Random()

    val changes = Seq(Seq(+1, -1), Seq(-1, +1))

    do {
      changed = false
      for (i <- lambdaValues.indices) {
        for (change <- changes(rng.nextInt(changes.size)) if lambdaValues(i) + change > 0) {
          lambdaValues(i) += change
          val newRuntime = RunGivenLambdas.run(n, bins, i => lambdaValues(i), lambdaValues, ollComputation)
          if (newRuntime < currentRuntime) {
            currentRuntime = newRuntime
            changed = true
            println(s"Updated to $currentRuntime with ${lambdaValues.mkString(", ")}")
          } else {
            lambdaValues(i) -= change
          }
        }
      }
    } while (changed)
  }
}
