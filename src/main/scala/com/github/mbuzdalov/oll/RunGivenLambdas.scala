package com.github.mbuzdalov.oll

import com.github.mbuzdalov.util.MathEx

object RunGivenLambdas {
  def defaultBins(n: Int): Seq[Int] = (0 to 30).map(log => n - (n >>> log)).distinct.sorted

  def run(n: Int, bins: Seq[Int], lambdas: Int => Double, ollComputation: OLLComputation): Double = {
    val runtimes = new Array[Double](n + 1)
    for (i <- bins.size - 2 to 0 by -1) {
      val lambda: Double = lambdas(i)
      for (f <- bins(i + 1) - 1 to bins(i) by -1) {
        runtimes(f) = ollComputation.findRuntime(f, lambda, runtimes)
      }
    }
    MathEx.expectedRuntimeOnBitStrings(n, runtimes)
  }

  def main(args: Array[String]): Unit = {
    val cmd = new CommandLineArgs(args)
    val n = cmd.getInt("n")
    val bins = defaultBins(n)
    val lambdas = cmd.getString("lambdas", "comma-separated sequence of lambda values")
      .split(',').map(_.toDouble)

    if (lambdas.length != bins.size - 1) {
      sys.error(s"The number of elements in --lambdas should be ${bins.size - 1}, which is derived from --n=$n")
    }

    val crossoverComputation = new InMemoryCostPrioritizingCrossoverCache(
      maxCacheByteSize = cmd.getLong("max-cache-byte-size"),
      delegate = CrossoverComputation,
      verbose = false)

    val ollComputation = new OLLComputation(n,
      neverMutateZeroBits = cmd.getBoolean("never-mutate-zero-bits"),
      includeBestMutantInComparison = cmd.getBoolean("include-best-mutant"),
      ignoreCrossoverParentDuplicates = cmd.getBoolean("ignore-crossover-parent-duplicates"),
      crossoverComputation = crossoverComputation)

    val result = run(n, bins, lambdas, ollComputation)
    println(result)
  }
}
