package com.github.mbuzdalov.opl.distribution

import com.github.mbuzdalov.opl.DoubleProbabilityVector

object FlipKBits extends ParameterizedDistribution[Int] {
  override def initialize(n: Int, param: Int, target: DoubleProbabilityVector): Unit = {
    target.setBounds(param, param)
    target.setValue(param, 1.0)
  }

  override def minimize(n: Int, fun: Int => Double): (Int, Double) = {
    var best, curr = 0
    var bestValue = Double.PositiveInfinity
    while (curr < n) {
      curr += 1
      val currValue = fun(curr)
      if (bestValue > currValue) {
        bestValue = currValue
        best = curr
      }
    }
    (best, bestValue)
  }
}
