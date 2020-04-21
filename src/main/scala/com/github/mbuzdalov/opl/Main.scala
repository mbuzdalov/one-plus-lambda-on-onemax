package com.github.mbuzdalov.opl

import java.nio.file.{Files, Paths}
import java.util.{ArrayList => JArrayList}
import java.util.concurrent.{Callable, Executors, TimeUnit}

import com.github.mbuzdalov.opl.analysis.{NonMonotonicityInDistanceSearch, NonMonotonicitySliceSearch}
import com.github.mbuzdalov.opl.computation.callback.Wrapper

import scala.jdk.CollectionConverters._
import scala.util.Using
import com.github.mbuzdalov.opl.computation.{DriftOptimalRunningTime, OptimalRunningTime}
import com.github.mbuzdalov.opl.distribution.{FlipKBits, ShiftBitMutation, StandardBitMutation}
import com.github.mbuzdalov.opl.picture.RelativeOptimalityPictureBuilder

object Main {
  def optimalValuesComparison(args: Array[String]): Unit = {
    val targetRoot = Paths.get(args(0))
    Files.createDirectories(targetRoot)
    val nValues = args(1).split(',').map(_.toInt)
    val maxLambda = args(2).toInt
    Using.resource(Files.newBufferedWriter(targetRoot.resolve(s"expected-times.csv"))) { times =>
      times.write("n,lambda,optimal,drift-optimal,standard-optimal,shift-optimal\n")
      for (n <- nValues; lambda <- 1 to maxLambda) {
        val t0 = timer(s"[n=$n, lambda=$lambda] Creating listeners...")
        val optimalListener = OptimalRunningTime.newListener(FlipKBits)
        val driftOptimalListener = DriftOptimalRunningTime.newListener(FlipKBits)
        val standardOptimalListener = OptimalRunningTime.newListener(StandardBitMutation)
        val shiftOptimalListener = OptimalRunningTime.newListener(ShiftBitMutation)
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

  def mainRuns(args: Array[String]): Unit = {
    val targetRoot = Paths.get(args(0))
    Files.createDirectories(targetRoot)
    val nValues = args(1).split(',').map(_.toInt)
    val lambdaValues = args(2).split(',').map(_.toInt)

    Using.resource(Files.newBufferedWriter(targetRoot.resolve("expectations.csv"))) { exps =>
      exps.write("n,lambda,optimal rls,drift optimal rls,optimal sbm,optimal shf\n")

      Using.resources(Files.newBufferedWriter(targetRoot.resolve("optimal-rls.csv")),
        Files.newBufferedWriter(targetRoot.resolve("drift-optimal-rls.csv")),
        Files.newBufferedWriter(targetRoot.resolve("optimal-sbm.csv")),
        Files.newBufferedWriter(targetRoot.resolve("optimal-shf.csv"))
      ) { (opt, drift, std, shf) =>
        opt.write("n,lambda,distance,value,time\n")
        drift.write("n,lambda,distance,value,time\n")
        std.write("n,lambda,distance,value,time\n")
        shf.write("n,lambda,distance,value,time\n")

        val theLock = new AnyRef

        class Task(n: Int, lambda: Int) extends Callable[Unit] {
          override def call(): Unit = {
            val allBitFlips = Array.tabulate(n)(i => i + 1)
            val probabilities = Array.tabulate(151)(i => math.pow(2, (i - 50) / 5.0) / n).filter(_ < 1)

            val wrapRLS, wrapRLSd = new Wrapper(allBitFlips)
            val wrapSBM, wrapSHF = new Wrapper(probabilities)

            Using.resources(
              new RelativeOptimalityPictureBuilder(targetRoot.resolve(s"optimal-rls-$n-$lambda.png"), 1, n, wrapRLS),
              new RelativeOptimalityPictureBuilder(targetRoot.resolve(s"optimal-sbm-$n-$lambda.png"), 1, n, wrapSBM),
              new RelativeOptimalityPictureBuilder(targetRoot.resolve(s"optimal-shf-$n-$lambda.png"), 1, n, wrapSHF),
            ) { (_, _, _) =>
              Using.resources(
                new NonMonotonicitySliceSearch(targetRoot.resolve(s"optimal-sbm-$n-$lambda.slice-non-monotone"), n / 2),
                new NonMonotonicitySliceSearch(targetRoot.resolve(s"optimal-shf-$n-$lambda.slice-non-monotone"), n / 2),
                new NonMonotonicityInDistanceSearch(targetRoot.resolve(s"optimal-rls-$n-$lambda.param-non-monotone")),
                new NonMonotonicityInDistanceSearch(targetRoot.resolve(s"drift-rls-$n-$lambda.param-non-monotone")),
              ) { (nmSBM, nmSHF, nmRLS, nmRLSd) =>
                wrapRLS.add(nmRLS)
                wrapSBM.add(nmSBM)
                wrapSHF.add(nmSHF)
                wrapRLSd.add(nmRLSd)

                val t0 = timer(s"[n=$n, lambda=$lambda] Creating listeners...")
                val optimalListener = OptimalRunningTime.newListener(FlipKBits, Some(wrapRLS))
                val optimalStandardListener = OptimalRunningTime.newListener(StandardBitMutation, Some(wrapSBM))
                val optimalShiftListener = OptimalRunningTime.newListener(ShiftBitMutation, Some(wrapSHF))
                val driftOptimalListener = DriftOptimalRunningTime.newListener(FlipKBits, Some(wrapRLSd))
                t0.done()

                val t1 = timer(s"[n=$n, lambda=$lambda] Running (1+lambda)...")
                OnePlusLambda(n, lambda, Seq(optimalListener, optimalStandardListener, optimalShiftListener, driftOptimalListener))
                t1.done()

                val optimalRLS = optimalListener.toResult
                val optimalRLSd= driftOptimalListener.toResult
                val optimalSBM = optimalStandardListener.toResult
                val optimalSHF = optimalShiftListener.toResult
                theLock.synchronized {
                  for (distance <- 1 to n) {
                    opt.write(s"$n,$lambda,$distance,${optimalRLS.optimalParameter(distance)},${optimalRLS.optimalExpectation(distance)}\n")
                    drift.write(s"$n,$lambda,$distance,${optimalRLSd.optimalParameter(distance)},${optimalRLSd.optimalExpectation(distance)}\n")
                    std.write(s"$n,$lambda,$distance,${optimalSBM.optimalParameter(distance)},${optimalSBM.optimalExpectation(distance)}\n")
                    shf.write(s"$n,$lambda,$distance,${optimalSHF.optimalParameter(distance)},${optimalSHF.optimalExpectation(distance)}\n")
                  }
                  exps.write(s"$n,$lambda,${optimalRLS.expectedRunningTime},${optimalRLSd.expectedRunningTime},${optimalSBM.expectedRunningTime},${optimalSHF.expectedRunningTime}\n")
                }
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

  def main(args: Array[String]): Unit = args(0) match {
    case "main" => mainRuns(args.tail)
    case "optimal-comparison" => optimalValuesComparison(args.tail)
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
