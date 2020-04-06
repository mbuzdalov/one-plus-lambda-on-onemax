package com.github.mbuzdalov.opl.computation

trait ComputationResult[@specialized P] extends BareComputationResult {
  def optimalParameter(distance: Int): P
  def optimalExpectationForParameter(distance: Int, parameter: P): Double
}
