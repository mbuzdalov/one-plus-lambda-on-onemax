package com.github.mbuzdalov.opl.distribution

import com.github.mbuzdalov.opl.DoubleProbabilityVector
import com.github.mbuzdalov.opl.MathEx.{logFactorial => lF}

object StandardBitMutation extends ParameterizedDistribution[Double] {
  override def initialize(n: Int, param: Double, target: DoubleProbabilityVector): Unit = {
    if (param == 0) {
      target.setBounds(1, 0)
    } else if (param == 1) {
      target.setBounds(n, n)
      target.setValue(n, 1.0)
    } else {
      val common = lF(n)
      val lP = math.log(param)
      val l1P = math.log1p(-param)
      target.setBounds(1, n)
      var i = 1
      while (i <= n) {
        target.setValue(i, math.exp(lP * i + l1P * (n - i) + common - lF(i) - lF(n - i)))
        i += 1
      }
    }
  }
}
