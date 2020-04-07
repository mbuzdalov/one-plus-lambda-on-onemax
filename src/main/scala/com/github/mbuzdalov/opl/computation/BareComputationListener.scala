package com.github.mbuzdalov.opl.computation

trait BareComputationListener {
  def startComputing(problemSize: Int, populationSize: Int): Unit
  def startDistance(distance: Int): Unit
  def startTransitionProbabilityGroup(distance: Int, change: Int): Unit
  def receiveTransitionProbability(change: Int, currDistance: Int, newDistance: Int, probability: Double): Unit
  def finishTransitionProbabilityGroup(distance: Int, change: Int): Unit
  def finishDistance(distance: Int): Unit
}
