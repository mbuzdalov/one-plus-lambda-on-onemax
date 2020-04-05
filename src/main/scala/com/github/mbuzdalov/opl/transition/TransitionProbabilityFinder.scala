package com.github.mbuzdalov.opl.transition

trait TransitionProbabilityFinder {
  type Aux
  def newAuxiliaryData(n: Int): Aux
  def find(n: Int, lambda: Int, distance: Int, change: Int, target: Array[Double], aux: Aux): Unit
}
