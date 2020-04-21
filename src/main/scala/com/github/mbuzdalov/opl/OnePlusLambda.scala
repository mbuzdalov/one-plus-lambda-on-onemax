package com.github.mbuzdalov.opl

import com.github.mbuzdalov.opl.computation.BareComputationListener
import com.github.mbuzdalov.opl.transition.DoubleProbabilityFinder

object OnePlusLambda {
  private val myFinder = DoubleProbabilityFinder

  def apply(n: Int, lambda: Int, listeners: Seq[BareComputationListener]): Unit = {
    val probabilities = new Array[Double](n)
    listeners.foreach(_.startComputing(n, lambda))
    for (distance <- 1 to n) {
      listeners.foreach(_.startDistance(distance))
      for (change <- 1 to n) {
        listeners.foreach(_.startTransitionProbabilityGroup(distance, change))
        val lower = math.max(change / 2 + 1, change - n + distance)
        val upper = math.min(change, distance)
        if (lower <= upper) {
          myFinder.find(n, distance, change, probabilities)
          for (okay <- lower to upper) {
            val newD = distance - 2 * okay + change
            val pi = probabilities(okay - lower)
            assert(newD < distance, s"n = $n, distance = $distance, change = $change, lower = $lower, upper = $upper, okay = $okay")
            listeners.foreach(_.receiveTransitionProbability(change, distance, newD, pi))
          }
        }
        listeners.foreach(_.finishTransitionProbabilityGroup(distance, change))
      }
      listeners.foreach(_.finishDistance(distance))
    }
  }
}
