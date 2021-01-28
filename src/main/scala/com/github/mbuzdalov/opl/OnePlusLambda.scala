package com.github.mbuzdalov.opl

import com.github.mbuzdalov.opl.computation.BareComputationListener
import com.github.mbuzdalov.opl.transition.DoubleTransitionMatrixFactory

object OnePlusLambda {
  private val myFinder = DoubleTransitionMatrixFactory

  def apply(n: Int, lambda: Int, listeners: Seq[BareComputationListener], printTimings: Boolean): Unit = {
    listeners.foreach(_.startComputing(n, lambda))
    for (distance <- 1 to n) {
      val t0 = System.nanoTime()
      val matrix = myFinder.create(n, distance)
      val t1 = System.nanoTime()
      if (printTimings)
        println(f"[n=$n%d, lambda=$lambda%d][distance = $distance%d] Matrix created in ${(t1 - t0) * 1e-9}%.03f s")
      listeners.foreach(_.processDistance(distance, matrix))
      val t2 = System.nanoTime()
      if (printTimings)
        println(f"[n=$n%d, lambda=$lambda%d][distance = $distance%d] Listeners done in ${(t2 - t1) * 1e-9}%.03f s")
    }
  }
}
