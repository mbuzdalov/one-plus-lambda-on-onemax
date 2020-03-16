package com.github.mbuzdalov.opl

import java.io.PrintWriter
import java.nio.file.Paths
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

  def powersOfTwo(cacheDirectoryName: String, processors: Int): Unit = {
    val cache = Paths.get(cacheDirectoryName)
    val tasks = new JArrayList[Callable[Unit]]()
    for (n <- Seq(1000, 2000, 10000)) {
      for (lLog <- 0 to 15; l = 1 << lLog) {
        val archive = new DeflatingListener(cache, "%d-%d.gz", n)
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

  def main(args: Array[String]): Unit = {
    args(0) match {
      case "drift-non-optimal" => driftNonOptimal(args(1))
      case "powers-of-two-populate" => powersOfTwo(args(1), args(2).toInt)
      case other => println(s"Command '$other' not recognized'")
    }
  }
}
