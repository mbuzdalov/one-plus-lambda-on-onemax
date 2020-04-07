package com.github.mbuzdalov.opl

import com.github.mbuzdalov.opl.computation.BareComputationListener
import com.github.mbuzdalov.opl.transition.BoundedProbabilityFinder

object OnePlusLambda {
  private val myFinder = BoundedProbabilityFinder

  def apply(n: Int, lambda: Int, listeners: Seq[BareComputationListener]): Unit = {
    val space = myFinder.newAuxiliaryData(n)
    val probabilities = new Array[Double](n)
    listeners.foreach(_.startComputing(n, lambda))
    for (distance <- 1 to n) {
      listeners.foreach(_.startDistance(distance))
      for (change <- 1 to n) {
        listeners.foreach(_.startTransitionProbabilityGroup(distance, change))
        val lower = math.max((change + 1) / 2, change - n + distance)
        val upper = math.min(change, distance)
        if (lower <= upper) {
          myFinder.find(n, lambda, distance, change, probabilities, space)
          for (okay <- lower to upper) {
            val newD = distance - 2 * okay + change
            val pi = probabilities(okay - lower)
            if (newD < distance) { // WTF? Should not be like this. The entire `transition` package should be updated
              listeners.foreach(_.receiveTransitionProbability(change, distance, newD, pi))
            }
          }
        }
        listeners.foreach(_.finishTransitionProbabilityGroup(distance, change))
      }
      listeners.foreach(_.finishDistance(distance))
    }
  }
}
