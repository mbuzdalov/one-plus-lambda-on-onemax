package com.github.mbuzdalov.oll

import java.util.concurrent.{Callable, ScheduledThreadPoolExecutor}

import com.github.mbuzdalov.oll.xover.LegacyCollectiveCrossoverComputation

object DemoPlots {
  private def varyingLastLambda(): Unit = {
    val n = 500
    val bins = RunGivenLambdas.defaultBins(n)
    val lambdas = Array(1.0, 1.0, 1.0, 1.0, 6.5, 8.5, 11.5, 16.5, 24.5)

    val crossoverComputation = new InMemoryCostPrioritizingCrossoverCache(
      maxCacheByteSize = 4000000000L, //cmd.getLong("max-cache-byte-size"),
      delegate = LegacyCollectiveCrossoverComputation,
      verbose = false)

    val ollComputation = new OLLComputation(n,
      neverMutateZeroBits = true, //cmd.getBoolean("never-mutate-zero-bits"),
      includeBestMutantInComparison = true, //cmd.getBoolean("include-best-mutant"),
      ignoreCrossoverParentDuplicates = true, //cmd.getBoolean("ignore-crossover-parent-duplicates"),
      crossoverComputation = crossoverComputation)

    println("lambda,popSize,runtime")

    val popSizes = lambdas.map(v => math.round(v).toInt)
    val lastIdx = lambdas.length - 1

    val tasks = new java.util.ArrayList[Callable[Unit]]

    for (last <- 10 to 300) {
      if (last % 10 == 5) {
        tasks.add(() => {
          val lambda = math.nextDown(last / 10.0)
          val myLambdas = lambdas.clone()
          val myPopSizes = popSizes.clone()
          myLambdas(lastIdx) = lambda
          myPopSizes(lastIdx) = math.round(lambda).toInt
          val result = RunGivenLambdas.run(n, bins, myLambdas, myPopSizes, ollComputation)
          DemoPlots.synchronized(println(s"$lambda,rounding,$result"))
        })
      }

      tasks.add(() => {
        val myLambdas = lambdas.clone()
        val myPopSizes = popSizes.clone()
        val lambda = last / 10.0
        myLambdas(lastIdx) = lambda
        myPopSizes(lastIdx) = math.round(lambda).toInt
        val result = RunGivenLambdas.run(n, bins, myLambdas, myPopSizes, ollComputation)
        DemoPlots.synchronized(println(s"$lambda,rounding,$result"))
      })
    }

    for (popSize <- 1 to 30) {
      for (lambda10 <- 10 to 300) {
        tasks.add(() => {
          val myLambdas = lambdas.clone()
          val myPopSizes = popSizes.clone()
          val lambda = lambda10 / 10.0
          myLambdas(lastIdx) = lambda
          myPopSizes(lastIdx) = popSize
          val result = RunGivenLambdas.run(n, bins, myLambdas, myPopSizes, ollComputation)
          DemoPlots.synchronized(println(s"$lambda,$popSize,$result"))
        })
      }
    }

    val pool = new ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
    pool.invokeAll(tasks)
    pool.shutdown()
  }

  def main(args: Array[String]): Unit = {
    varyingLastLambda()
  }
}
