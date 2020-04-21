package com.github.mbuzdalov.opl.distribution

import com.github.mbuzdalov.opl.DoubleProbabilityVector

object FlipKBits extends ParameterizedDistribution[Int] {
  override def initialize(n: Int, param: Int, target: DoubleProbabilityVector): Unit = {
    target.setBounds(param, param)
    target.setValue(param, 1.0)
  }
}
