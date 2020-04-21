package com.github.mbuzdalov.opl

import com.github.mbuzdalov.opl.computation.BareComputationListener
import com.github.mbuzdalov.opl.transition.DoubleProbabilityFinder

object OnePlusLambda {
  private val myFinder = DoubleProbabilityFinder

  def apply(n: Int, lambda: Int, listeners: Seq[BareComputationListener]): Unit = {
    val probabilities = new DoubleProbabilityVector(n)
    listeners.foreach(_.startComputing(n, lambda))
    for (distance <- 1 to n) {
      listeners.foreach(_.startDistance(distance))
      for (change <- 1 to n) {
        listeners.foreach(_.startTransitionProbabilityGroup(distance, change))
        myFinder.find(n, distance, change, probabilities)
        if (probabilities.nonEmpty) {
          val lower = probabilities.smallestDistance
          val upper = probabilities.largestDistance
          for (okay <- lower to upper) {
            val newD = distance - 2 * okay + change
            val pi = probabilities.probability(okay)
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
