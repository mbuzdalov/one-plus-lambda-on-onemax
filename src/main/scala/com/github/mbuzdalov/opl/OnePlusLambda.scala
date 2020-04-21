package com.github.mbuzdalov.opl

import com.github.mbuzdalov.opl.computation.BareComputationListener
import com.github.mbuzdalov.opl.transition.DoubleTransitionMatrixFactory

object OnePlusLambda {
  private val myFinder = DoubleTransitionMatrixFactory

  def apply(n: Int, lambda: Int, listeners: Seq[BareComputationListener]): Unit = {
    listeners.foreach(_.startComputing(n, lambda))
    for (distance <- 1 to n) {
      val matrix = myFinder.create(n, distance)
      listeners.foreach(_.processDistance(distance, matrix))
    }
  }
}
