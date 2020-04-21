package com.github.mbuzdalov.opl.computation

trait ComputationResult[@specialized P] extends BareComputationResult {
  def optimalParameter(distance: Int): P
}
