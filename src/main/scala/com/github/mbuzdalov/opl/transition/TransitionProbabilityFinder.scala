package com.github.mbuzdalov.opl.transition

import com.github.mbuzdalov.opl.DoubleProbabilityVector

trait TransitionProbabilityFinder {
  def find(n: Int, distance: Int, change: Int, target: DoubleProbabilityVector): Unit
}
