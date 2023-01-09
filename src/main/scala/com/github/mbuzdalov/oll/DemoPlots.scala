package com.github.mbuzdalov.oll

object DemoPlots {
  private def varyingLastLambda(): Unit = {
    val n = 500
    val bins = RunGivenLambdas.defaultBins(n)
    val lambdas = Array(1.0, 1.0, 1.0, 1.0, 6.5, 8.5, 11.5, 16.5, 24.5)

    val crossoverComputation = new InMemoryCostPrioritizingCrossoverCache(
      maxCacheByteSize = 4000000000L, //cmd.getLong("max-cache-byte-size"),
      delegate = CrossoverComputation,
      verbose = false)

    val ollComputation = new OLLComputation(n,
      neverMutateZeroBits = true, //cmd.getBoolean("never-mutate-zero-bits"),
      includeBestMutantInComparison = true, //cmd.getBoolean("include-best-mutant"),
      ignoreCrossoverParentDuplicates = true, //cmd.getBoolean("ignore-crossover-parent-duplicates"),
      crossoverComputation = crossoverComputation)

    println("lambda,popSize,runtime")

    val popSizes = lambdas.map(v => math.round(v).toInt)
    val lastIdx = lambdas.length - 1

    for (last <- 10 to 300) {
      if (last % 10 == 5) {
        val ulpLambda = math.nextDown(last / 10.0)
        lambdas(lastIdx) = ulpLambda
        popSizes(lastIdx) = math.round(ulpLambda).toInt
        println(s"$ulpLambda,rounding,${RunGivenLambdas.run(n, bins, lambdas, popSizes, ollComputation)}")
      }
      val lastLambda = last / 10.0
      lambdas(lastIdx) = lastLambda
      popSizes(lastIdx) = math.round(lastLambda).toInt
      println(s"$lastLambda,rounding,${RunGivenLambdas.run(n, bins, lambdas, popSizes, ollComputation)}")
    }

    for (popSize <- 1 to 30) {
      for (delta <- -10 to 10) {
        val lambda = popSize + delta / 10.0
        if (lambda >= 1) {
          lambdas(lastIdx) = lambda
          popSizes(lastIdx) = popSize
          println(s"$lambda,$popSize,${RunGivenLambdas.run(n, bins, lambdas, popSizes, ollComputation)}")
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    varyingLastLambda()
  }
}
