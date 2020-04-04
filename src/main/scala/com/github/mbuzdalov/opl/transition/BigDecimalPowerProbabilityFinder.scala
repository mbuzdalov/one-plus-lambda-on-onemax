package com.github.mbuzdalov.opl.transition

import com.github.mbuzdalov.opl.MathEx.{logFactorialBig => lF}
import spire.implicits._

object BigDecimalPowerProbabilityFinder extends TransitionProbabilityFinder {
  override def find(n: Int, lambda: Int, d: Int, change: Int, target: Array[Double]): Unit = {
    val l = math.max((change + 1) / 2, change - n + d)
    val u = math.min(change, d)

    val common = lF(d) + lF(n - d) - lF(n) + lF(change) + lF(n - change)
    val tmp = new Array[BigDecimal](u - l + 1)
    var k = 0
    var sum = lF(0)
    while (k <= u - l) {
      val v = (common - lF(k + l) - lF(d - k - l) - lF(change - k - l) - lF(n - d - change + k + l)).exp()
      sum += v
      tmp(k) = v
      k += 1
    }

    sum = 1 - sum
    var lastPow = sum.pow(lambda)
    k = 0
    while (k <= u - l) {
      val v = tmp(k)
      sum += v
      val currPow = sum.pow(lambda)
      target(k) = (currPow - lastPow).toDouble
      lastPow = currPow
      k += 1
    }
  }
}
