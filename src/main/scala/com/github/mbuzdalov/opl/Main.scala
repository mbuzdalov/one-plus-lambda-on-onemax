package com.github.mbuzdalov.opl

import java.io.PrintWriter
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
          val opl = new OnePlusLambda(n, l)
          val optimal = opl.optimalExpectedTime
          val driftOptimal = opl.driftOptimalExpectedTime
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

  def main(args: Array[String]): Unit = {
    args(0) match {
      case "drift-non-optimal" => driftNonOptimal(args(1))
      case other => println(s"Command '$other' not recognized'")
    }
  }
}
