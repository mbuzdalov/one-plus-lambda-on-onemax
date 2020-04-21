package com.github.mbuzdalov.opl.distribution

import com.github.mbuzdalov.opl.DoubleProbabilityVector

trait ParameterizedDistribution[@specialized P] {
  def initialize(n: Int, param: P, target: DoubleProbabilityVector): Unit
}
