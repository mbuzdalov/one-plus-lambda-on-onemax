package com.github.mbuzdalov.oll

import java.io.PrintWriter
import java.util.concurrent.{Callable, ScheduledThreadPoolExecutor}

import scala.jdk.CollectionConverters._
import scala.util.Using

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

    val popSizes = lambdas.map(v => math.round(v).toInt)
    val lastIdx = lambdas.length - 1

    case class Result(popSize: Int, lambda: Double, result: Double)
    val maxPopSize = 40
    val allLastLambdas = ((10 to maxPopSize * 10).map(_ / 10.0) ++ (10 to maxPopSize * 10).filter(_ % 10 == 5).map(v => math.nextDown(v / 10.0))).sorted
    val allPopSizes = 1 to maxPopSize

    val tasks = new java.util.ArrayList[Callable[Result]]

    for (popSize <- allPopSizes; lastLambda <- allLastLambdas) {
      tasks.add(() => {
        val myLambdas = lambdas.clone()
        val myPopSizes = popSizes.clone()
        myLambdas(lastIdx) = lastLambda
        myPopSizes(lastIdx) = popSize
        val result = RunGivenLambdas.run(n, bins, myLambdas, myPopSizes, ollComputation)
        System.err.println(s"[debug] popSize = $popSize, lastLambda = $lastLambda, result = $result")
        Result(popSize, lastLambda, result)
      })
    }

    val pool = new ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
    val results = pool.invokeAll(tasks).asScala.map(_.get())
    pool.shutdown()

    Using.resource(new PrintWriter("oll-binning-n500-last-lambda-v1.csv")) { out =>
      out.println("lambda,popSize,runtime")
      for (Result(popSize, lambda, result) <- results) {
        out.println(s"$lambda,$popSize,$result")
        if (math.round(lambda) == popSize) {
          out.println(s"$lambda,rounding,$result")
        }
      }
    }
    Using.resource(new PrintWriter("oll-binning-n500-last-lambda-v2.csv")) { out =>
      out.println(allPopSizes.mkString("lambda,", ",", ",rounding"))
      for (lastLambda <- allLastLambdas) {
        val resultsFiltered = results.filter(_.lambda == lastLambda)
        assert(allPopSizes.indices.forall(i => allPopSizes(i) == resultsFiltered(i).popSize), resultsFiltered.map(_.popSize).mkString(","))
        val roundedPopSize = math.round(lastLambda).toInt
        out.println(resultsFiltered.map(_.result).mkString(s"$lastLambda,", ",", s",${resultsFiltered(roundedPopSize - 1).result}"))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    varyingLastLambda()
  }
}
