package com.github.mbuzdalov.opl.transition

trait TransitionProbabilityFinder {
  def find(n: Int, distance: Int, change: Int, target: TransitionProbabilities): Unit
}
