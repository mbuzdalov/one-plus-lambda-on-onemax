package com.github.mbuzdalov.opl.computation

trait ComputationListener[P] extends BareComputationListener {
  def toResult: ComputationResult[P]
}
