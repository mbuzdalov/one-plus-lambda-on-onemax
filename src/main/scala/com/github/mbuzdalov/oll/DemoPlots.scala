package com.github.mbuzdalov.oll

import java.awt.image.BufferedImage
import java.io.PrintWriter
import java.nio.file.{Files, Paths}
import java.util.concurrent.{Callable, ScheduledThreadPoolExecutor}

import javax.imageio.ImageIO

import scala.jdk.CollectionConverters._
import scala.util.Using

import com.github.mbuzdalov.oll.xover.LegacyCollectiveCrossoverComputation
import com.github.mbuzdalov.util.Viridis

object DemoPlots {
  private def varyingLastTwoLambdas(): Unit = {
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
    val idx1 = lambdas.length - 1
    val idx2 = lambdas.length - 2

    val minLambda1 = 20
    val maxLambda1 = 30
    val minLambda2 = 12
    val maxLambda2 = 22

    case class Result(lambda1: Double, lambda1Idx: Int, lambda2: Double, lambda2Idx: Int, result: Double)

    val tasks = new java.util.ArrayList[Callable[Result]]
    val lambdaCount = 100
    for (lambda1Idx <- 0 until lambdaCount; lambda2Idx <- 0 until lambdaCount) {
      tasks.add(() => {
        val lambda1 = minLambda1 + (lambda1Idx + 0.5) / lambdaCount * (maxLambda1 - minLambda1)
        val lambda2 = minLambda2 + (lambda2Idx + 0.5) / lambdaCount * (maxLambda2 - minLambda2)
        val myLambdas = lambdas.clone()
        val myPopSizes = popSizes.clone()
        myLambdas(idx1) = lambda1
        myPopSizes(idx1) = math.round(lambda1).toInt
        myLambdas(idx2) = lambda2
        myPopSizes(idx2) = math.round(lambda2).toInt
        val result = RunGivenLambdas.run(n, bins, myLambdas, myPopSizes, ollComputation)
        println(s"lambda1 = $lambda1, lambda2 = $lambda2, result = $result")
        Result(lambda1, lambda1Idx, lambda2, lambda2Idx, result)
      })
    }

    val pool = new ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
    val results = pool.invokeAll(tasks).asScala.map(_.get())
    pool.shutdown()

    val resultMin = results.minBy(_.result).result
    val resultMax = results.maxBy(_.result).result
    println(s"min = $resultMin, max = $resultMax")

    val image = new BufferedImage(lambdaCount, lambdaCount, BufferedImage.TYPE_INT_RGB)
    for (Result(_, lambda1Idx, _, lambda2Idx, result) <- results) {
      val normalized = (result - resultMin) / (resultMax - resultMin)
      image.setRGB(lambda1Idx, lambdaCount - lambda2Idx - 1, Viridis(normalized))
    }
    Using.resource(Files.newOutputStream(Paths.get("two-lambdas.png")))(stream => ImageIO.write(image, "png", stream))
  }

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
    //varyingLastLambda()
    varyingLastTwoLambdas()
  }
}
