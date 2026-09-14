package com.github.mbuzdalov.oll

import com.github.mbuzdalov.util.Loops.loopFromDownTo
import com.github.mbuzdalov.util.MathEx

object RunGivenLambdas:
  def defaultBins(n: Int): Seq[Int] = (0 to 30).map(log => n - (n >>> log)).distinct.sorted

  def run(n: Int, bins: Seq[Int], lambdas: Int => Double, populationSizes: Int => Int, ollComputation: OLLComputation): Double =
    val runtimes = Array.ofDim[Double](n + 1)
    loopFromDownTo(bins.size - 2, 0): i =>
      val lambda = lambdas(i)
      val populationSize = populationSizes(i)
      loopFromDownTo(bins(i + 1) - 1, bins(i)): f =>
        runtimes(f) = ollComputation.findRuntime(f, lambda, populationSize, runtimes).toDouble
    MathEx.expectedRuntimeOnBitStrings(n, runtimes)

  def runSmooth(n: Int, bins: Seq[Int], lambdas: Int => Double, populationSizes: Int => Double, ollComputation: OLLComputation): Double =
    val runtimes = Array.ofDim[Double](n + 1)
    loopFromDownTo(bins.size - 2, 0): i =>
      val lambda = lambdas(i)
      val populationSize = populationSizes(i)
      val populationSizeSmallV = math.floor(populationSize).toInt
      val populationSizeLargeV = math.ceil(populationSize).toInt
      if populationSizeSmallV == populationSizeLargeV then
        loopFromDownTo(bins(i + 1) - 1, bins(i)): f =>
          runtimes(f) = ollComputation.findRuntime(f, lambda, populationSizeSmallV, runtimes).toDouble
      else
        val probabilitySmall = populationSizeLargeV - populationSize
        val probabilityLarge = populationSize - populationSizeSmallV

        loopFromDownTo(bins(i + 1) - 1, bins(i)): f =>
          val resultSmall = ollComputation.findRuntime(f, lambda, populationSizeSmallV, runtimes)
          val resultLarge = ollComputation.findRuntime(f, lambda, populationSizeLargeV, runtimes)
          runtimes(f) = (resultSmall * probabilitySmall + resultLarge * probabilityLarge).toDouble
    MathEx.expectedRuntimeOnBitStrings(n, runtimes)

  def main(args: Array[String]): Unit =
    val cmd = CommandLineArgs(args)
    val n = cmd.getInt("n")
    val bins = defaultBins(n)
    val lambdas = cmd.getString("lambdas", "comma-separated sequence of lambda values")
      .split(',').map(_.toDouble)

    if lambdas.length != bins.size - 1 then
      sys.error(s"The number of elements in --lambdas should be ${bins.size - 1}, which is derived from --n=$n")

    val crossoverComputation = InMemoryCostPrioritizingCrossoverCache(
      maxCacheByteSize = cmd.getLong("max-cache-byte-size"),
      delegate = CrossoverComputation.findMathCapableImplementation(cmd, "crossover-math"),
      verbose = true)

    val ollComputation = OLLComputation(n,
      neverMutateZeroBits = cmd.getBoolean("never-mutate-zero-bits"),
      includeBestMutantInComparison = cmd.getBoolean("include-best-mutant"),
      ignoreCrossoverParentDuplicates = cmd.getBoolean("ignore-crossover-parent-duplicates"),
      crossoverComputation = crossoverComputation)

    val t0 = System.currentTimeMillis()
    val result = run(n, bins, lambdas, i => math.round(lambdas(i)).toInt, ollComputation)
    val time = System.currentTimeMillis() - t0
    println(s"$result in $time ms")
  end main
