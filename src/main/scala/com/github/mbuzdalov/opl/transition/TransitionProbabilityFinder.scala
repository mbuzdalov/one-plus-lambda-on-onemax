package com.github.mbuzdalov.opl.transition

trait TransitionProbabilityFinder {
  def find(n: Int, lambda: Int, distance: Int, change: Int, target: Array[Double]): Unit
}
