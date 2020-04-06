package com.github.mbuzdalov.opl

import java.nio.file.{Files, Path, Paths}

import scala.util.Using

import com.github.mbuzdalov.opl.computation.{DriftOptimalRunningTime, OptimalRunningTime}
import com.github.mbuzdalov.opl.picture.RelativeOptimalityPictureBuilder

object Main {
  def optimalValuesComparison(args: Array[String]): Unit = {
    val targetRoot = Paths.get(args(0))
    Files.createDirectories(targetRoot)
    val nValues = args(1).split(',').map(_.toInt)
    val maxLambda = args(2).toInt
    Using.resource(Files.newBufferedWriter(targetRoot.resolve(s"expected-times.csv"))) { times =>
      times.write("n,lambda,optimal,drift-optimal\n")
      for (n <- nValues; lambda <- 1 to maxLambda) {
        val t0 = timer(s"[n=$n, lambda=$lambda] Creating listeners...")
        val optimalListener = OptimalRunningTime.newListener
        val driftOptimalListener = DriftOptimalRunningTime.newListener
        t0.done()

        val t1 = timer(s"[n=$n, lambda=$lambda] Running (1+lambda)...")
        OnePlusLambda(n, lambda, Seq(optimalListener, driftOptimalListener))
        val optimalResult = optimalListener.toResult
        val driftOptimalResult = driftOptimalListener.toResult
        t1.done()

        val t2 = timer(s"[n=$n, lambda=$lambda] Computing and writing expectations...")
        times.write(s"$n,$lambda,${optimalResult.expectedRunningTime},${driftOptimalResult.expectedRunningTime}\n")
        t2.done()
      }
    }
  }

  def pictures(args: Array[String]): Unit = {
    val targetRoot = Paths.get(args(0))
    Files.createDirectories(targetRoot)
    val nValues = args(1).split(',').map(_.toInt)
    val lambdaValues = args(2).split(',').map(_.toInt)
    for (n <- nValues) {
      val allBitFlips = Array.tabulate(n)(i => i + 1)
      for (lambda <- lambdaValues) {
        val t0 = timer(s"[n=$n, lambda=$lambda] Creating listeners...")
        val optimalListener = OptimalRunningTime.newListener
        val driftOptimalListener = DriftOptimalRunningTime.newListener
        t0.done()

        val t1 = timer(s"[n=$n, lambda=$lambda] Running (1+lambda)...")
        OnePlusLambda(n, lambda, Seq(optimalListener, driftOptimalListener))
        val optimalResult = optimalListener.toResult
        val driftOptimalResult = driftOptimalListener.toResult
        t1.done()

        val t3 = timer(s"[n=$n, lambda=$lambda] Computing and writing optimal picture...")
        RelativeOptimalityPictureBuilder(source = optimalResult,
                                         optimalitySource = optimalResult,
                                         ordinateValues = allBitFlips,
                                         xMin = 1, xMax = n,
                                         target = targetRoot.resolve(s"optimal-$n-$lambda.png"))
        t3.done()

        val t4 = timer(s"[n=$n, lambda=$lambda] Computing and writing drift-optimal picture...")
        RelativeOptimalityPictureBuilder(source = driftOptimalResult,
                                         optimalitySource = optimalResult,
                                         ordinateValues = allBitFlips,
                                         xMin = 1, xMax = n,
                                         target = targetRoot.resolve(s"drift-optimal-$n-$lambda.png"))
        t4.done()
      }
    }
  }

  def main(args: Array[String]): Unit = args(0) match {
    case "pictures" => pictures(args.tail)
    case "optimal-comparison" => optimalValuesComparison(args.tail)
  }

  case class timer(message: String) {
    private val t0 = System.nanoTime()
    print(message)

    def done(): Unit = {
      println(f" done in ${(System.nanoTime() - t0) * 1e-9}%.3f s")
    }
  }
}
