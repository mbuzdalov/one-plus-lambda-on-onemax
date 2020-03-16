package com.github.mbuzdalov.opl

import java.io.PrintWriter
import java.nio.file.{Files, Paths}
import java.util.concurrent.{Callable, Executors}
import java.util.{ArrayList => JArrayList}

object Main {
  def driftNonOptimal(outputFileName: String): Unit = {
    val tasks = new JArrayList[Callable[Unit]]()
    val out = new PrintWriter(outputFileName)
    out.println("n,l,optimal,drift-optimal,iteration-loss,evaluation-loss")
    out.flush()

    for (n <- 500 to 2000 by 500) {
      for (l <- 1 to 2000) {
        tasks.add(() => {
          val summary = new SummaryOnlyListener
          new OnePlusLambda(n, l, summary)
          val optimal = summary.expectedOptimalTime
          val driftOptimal = summary.expectedDriftOptimalTime
          out.println(s"$n,$l,$optimal,$driftOptimal,${driftOptimal - optimal},${(driftOptimal - optimal) * l}")
        })
      }
    }

    val pool = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    val futures = pool.invokeAll(tasks)
    futures.forEach(_.get())
    pool.shutdown()
    out.close()
  }

  def populatePowersOfTwo(cacheDirectoryName: String, processors: Int): Unit = {
    val cache = Paths.get(cacheDirectoryName)
    val tasks = new JArrayList[Callable[Unit]]()
    for (n <- Seq(1000, 2000, 10000)) {
      for (lLog <- 0 to 15; l = 1 << lLog) {
        val archive = new DeflatingListener(cache.resolve(s"$n-$l.gz"))
        tasks.add(() => {
          val summary = new SummaryOnlyListener
          new OnePlusLambda(n, l, archive ++ summary)
          println(s"$n, $l => ${summary.expectedOptimalTime}")
        })
      }
    }

    val pool = Executors.newFixedThreadPool(processors)
    val futures = pool.invokeAll(tasks)
    futures.forEach(_.get())
    pool.shutdown()
  }

  def buildReport(cacheDirectoryName: String, resultDirectoryName: String): Unit = {
    val cache = Paths.get(cacheDirectoryName)
    val results = Paths.get(resultDirectoryName)
    for (n <- Seq(1000, 2000, 10000)) {
      for (lLog <- 0 to 15; l = 1 << lLog) {
        val archive = cache.resolve(s"$n-$l.gz")
        if (Files.exists(archive)) {
          val pictureListener = new EllPictureBuildingListener(results.resolve(s"ell-$n-$l.png"))
          Inflater.apply(n, l, archive, pictureListener)
          println(s"$archive processed")
        } else {
          println(s"Warning: could not find $archive")
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    args(0) match {
      case "drift-non-optimal" => driftNonOptimal(args(1))
      case "powers-of-two-populate" => populatePowersOfTwo(args(1), args(2).toInt)
      case "build-report" => buildReport(args(1), args(2))
      case other => println(s"Command '$other' not recognized'")
    }
  }
}
