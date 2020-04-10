package com.github.mbuzdalov.opl

import java.nio.file.{Files, Paths}
import java.util.{ArrayList => JArrayList}
import java.util.concurrent.{Callable, Executors, TimeUnit}

import scala.jdk.CollectionConverters._
import scala.util.Using

import com.github.mbuzdalov.opl.computation.{DriftOptimalRunningTime, OptimalMutationRunningTime, OptimalRunningTime}
import com.github.mbuzdalov.opl.picture.RelativeOptimalityPictureBuilder

object Main {
  def optimalParameterValues(args: Array[String]): Unit = {
    val targetRoot = Paths.get(args(0))
    Files.createDirectories(targetRoot)
    val nValues = args(1).split(',').map(_.toInt)
    val lambdaValues = args(2).split(',').map(_.toInt)
    Using.resources(Files.newBufferedWriter(targetRoot.resolve("optimal-flips.csv")),
                    Files.newBufferedWriter(targetRoot.resolve("drift-optimal-flips.csv")),
                    Files.newBufferedWriter(targetRoot.resolve("optimal-mutation-rate-std.csv")),
                    Files.newBufferedWriter(targetRoot.resolve("optimal-mutation-rate-shift.csv"))) { (opt, drift, std, shf) =>
      opt.write("n,lambda,distance,value\n")
      drift.write("n,lambda,distance,value\n")
      std.write("n,lambda,distance,value\n")
      shf.write("n,lambda,distance,value\n")
      Using.resource(Files.newBufferedWriter(targetRoot.resolve("strangeness.txt"))) { strange =>
        val theLock = new AnyRef

        class Task(n: Int, lambda: Int) extends Callable[Unit] {
          override def call(): Unit = {
            val t0 = timer(s"[n=$n, lambda=$lambda] Creating listeners...")
            val optimalListener = OptimalRunningTime.newListener
            val driftOptimalListener = DriftOptimalRunningTime.newListener
            val standardOptimalListener = OptimalMutationRunningTime.newListener(false)
            val shiftOptimalListener = OptimalMutationRunningTime.newListener(true)
            t0.done()

            val t1 = timer(s"[n=$n, lambda=$lambda] Running (1+lambda)...")
            OnePlusLambda(n, lambda, Seq(optimalListener, driftOptimalListener, standardOptimalListener, shiftOptimalListener))
            val optimalResult = optimalListener.toResult
            val driftOptimalResult = driftOptimalListener.toResult
            val standardOptimalResult = standardOptimalListener.toResult
            val shiftOptimalResult = shiftOptimalListener.toResult
            t1.done()

            var prevOptP = -1
            for (distance <- 1 to n) {
              val optP = optimalResult.optimalParameter(distance)
              if (optP < prevOptP) {
                val prevP = optimalResult.optimalExpectationForParameter(distance - 1, prevOptP)
                val prevC = optimalResult.optimalExpectationForParameter(distance - 1, optP)
                val currP = optimalResult.optimalExpectationForParameter(distance, prevOptP)
                val currC = optimalResult.optimalExpectationForParameter(distance, optP)
                theLock.synchronized {
                  strange.write(s"n=$n, lambda=$lambda: ${distance - 1}: $prevOptP => $prevP, $optP => $prevC. $distance: $prevOptP => $currP, $optP => $currC\n")
                }
              }
              prevOptP = optP
              theLock.synchronized {
                opt.write(s"$n,$lambda,$distance,$optP\n")
                drift.write(s"$n,$lambda,$distance,${driftOptimalResult.optimalParameter(distance)}\n")
                std.write(s"$n,$lambda,$distance,${standardOptimalResult.optimalParameter(distance)}\n")
                shf.write(s"$n,$lambda,$distance,${shiftOptimalResult.optimalParameter(distance)}\n")
              }
            }
          }
        }

        val tasks = new JArrayList[Task]()
        for (n <- nValues; lambda <- lambdaValues) tasks.add(new Task(n, lambda))
        val pool = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
        val futures = pool.invokeAll(tasks)
        futures.asScala.foreach(_.get())
        pool.shutdown()
        pool.awaitTermination(1, TimeUnit.HOURS)
      }
    }
  }

  def optimalValuesComparison(args: Array[String]): Unit = {
    val targetRoot = Paths.get(args(0))
    Files.createDirectories(targetRoot)
    val nValues = args(1).split(',').map(_.toInt)
    val maxLambda = args(2).toInt
    Using.resource(Files.newBufferedWriter(targetRoot.resolve(s"expected-times.csv"))) { times =>
      times.write("n,lambda,optimal,drift-optimal,standard-optimal,shift-optimal\n")
      for (n <- nValues; lambda <- 1 to maxLambda) {
        val t0 = timer(s"[n=$n, lambda=$lambda] Creating listeners...")
        val optimalListener = OptimalRunningTime.newListener
        val driftOptimalListener = DriftOptimalRunningTime.newListener
        val standardOptimalListener = OptimalMutationRunningTime.newListener(false)
        val shiftOptimalListener = OptimalMutationRunningTime.newListener(true)
        t0.done()

        val t1 = timer(s"[n=$n, lambda=$lambda] Running (1+lambda)...")
        OnePlusLambda(n, lambda, Seq(optimalListener, driftOptimalListener, standardOptimalListener, shiftOptimalListener))
        val optimalResult = optimalListener.toResult
        val driftOptimalResult = driftOptimalListener.toResult
        val standardOptimalResult = standardOptimalListener.toResult
        val shiftOptimalResult = shiftOptimalListener.toResult
        t1.done()

        val t2 = timer(s"[n=$n, lambda=$lambda] Computing and writing expectations...")
        times.write(s"$n,$lambda,${optimalResult.expectedRunningTime},${driftOptimalResult.expectedRunningTime},")
        times.write(s"${standardOptimalResult.expectedRunningTime},${shiftOptimalResult.expectedRunningTime}\n")
        t2.done()
      }
    }
  }

  def pictures(args: Array[String]): Unit = {
    val targetRoot = Paths.get(args(0))
    Files.createDirectories(targetRoot)
    val nValues = args(1).split(',').map(_.toInt)
    val lambdaValues = args(2).split(',').map(_.toInt)

    class Task(n: Int, lambda: Int) extends Callable[Unit] {
      override def call(): Unit = {
        val allBitFlips = Array.tabulate(n)(i => i + 1)
        val probabilities = Array.tabulate(201)(i => math.pow(2, (i - 100) / 5.0) / n).filter(_ < 1)

        val t0 = timer(s"[n=$n, lambda=$lambda] Creating listeners...")
        val optimalListener = OptimalRunningTime.newListener
        val driftOptimalListener = DriftOptimalRunningTime.newListener
        val optimalStandardListener = OptimalMutationRunningTime.newListener(false)
        val optimalShiftListener = OptimalMutationRunningTime.newListener(true)
        t0.done()

        val t1 = timer(s"[n=$n, lambda=$lambda] Running (1+lambda)...")
        OnePlusLambda(n, lambda, Seq(optimalListener, driftOptimalListener, optimalStandardListener, optimalShiftListener))
        val optimalResult = optimalListener.toResult
        val driftOptimalResult = driftOptimalListener.toResult
        val optimalStandardResult = optimalStandardListener.toResult
        val optimalShiftResult = optimalShiftListener.toResult
        t1.done()

        val t3 = timer(s"[n=$n, lambda=$lambda] Computing and writing optimal picture...")
        RelativeOptimalityPictureBuilder(
          source = optimalResult,
          optimalitySource = optimalResult,
          ordinateValues = allBitFlips,
          xMin = 1, xMax = n,
          target = targetRoot.resolve(s"optimal-$n-$lambda.png")
        )
        t3.done()

        val t4 = timer(s"[n=$n, lambda=$lambda] Computing and writing drift-optimal picture...")
        RelativeOptimalityPictureBuilder(
          source = driftOptimalResult,
          optimalitySource = optimalResult,
          ordinateValues = allBitFlips,
          xMin = 1, xMax = n,
          target = targetRoot.resolve(s"drift-optimal-$n-$lambda.png")
        )
        t4.done()

        val t5 = timer(s"[n=$n, lambda=$lambda] Computing and writing relative optimal standard-bit mutation picture...")
        RelativeOptimalityPictureBuilder(
          source = optimalStandardResult,
          optimalitySource = optimalStandardResult,
          ordinateValues = probabilities,
          xMin = 1, xMax = n,
          target = targetRoot.resolve(s"standard-optimal-$n-$lambda.png")
        )
        t5.done()

        val t6 = timer(s"[n=$n, lambda=$lambda] Computing and writing relative optimal shift mutation picture...")
        RelativeOptimalityPictureBuilder(
          source = optimalShiftResult,
          optimalitySource = optimalShiftResult,
          ordinateValues = probabilities,
          xMin = 1, xMax = n,
          target = targetRoot.resolve(s"shift-optimal-$n-$lambda.png")
        )
        t6.done()
      }
    }

    val tasks = new JArrayList[Task]()
    for (n <- nValues; lambda <- lambdaValues) tasks.add(new Task(n, lambda))
    val pool = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    val futures = pool.invokeAll(tasks)
    futures.asScala.foreach(_.get())
    pool.shutdown()
    pool.awaitTermination(1, TimeUnit.HOURS)
  }

  def main(args: Array[String]): Unit = args(0) match {
    case "pictures" => pictures(args.tail)
    case "optimal-comparison" => optimalValuesComparison(args.tail)
    case "optimal-parameters" => optimalParameterValues(args.tail)
  }

  case class timer(message: String) {
    private val t0 = System.nanoTime()

    def done(): Unit = {
      Main.synchronized {
        println(f"$message done in ${(System.nanoTime() - t0) * 1e-9}%.3f s")
      }
    }
  }
}
