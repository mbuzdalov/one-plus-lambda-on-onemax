package com.github.mbuzdalov.opl.distribution

import com.github.mbuzdalov.opl.DoubleProbabilityVector
import com.github.mbuzdalov.util.Loops.loopFromTo

object FlipKBits extends ParameterizedDistribution[Int]:
  override def initialize(n: Int, param: Int, target: DoubleProbabilityVector): Unit =
    target.setBounds(param, param)
    target.setValue(param, 1.0)

  override def minimize(n: Int, fun: Int => Double): (Int, Double) =
    var best = 0
    var bestValue = Double.PositiveInfinity
    loopFromTo(1, n): curr =>
      val currValue = fun(curr)
      if bestValue > currValue then
        bestValue = currValue
        best = curr
    (best, bestValue)
