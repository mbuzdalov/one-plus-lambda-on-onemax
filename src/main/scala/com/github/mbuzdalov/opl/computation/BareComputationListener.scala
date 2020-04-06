package com.github.mbuzdalov.opl.computation

trait BareComputationListener {
  def startComputing(problemSize: Int, populationSize: Int): Unit
  def startDistance(distance: Int): Unit
  def startTransitionProbabilityGroup(change: Int): Unit
  def receiveTransitionProbability(newDistance: Int, probability: Double): Unit
  def finishTransitionProbabilityGroup(): Unit
  def finishDistance(): Unit
}
