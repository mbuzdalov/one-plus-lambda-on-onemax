package com.github.mbuzdalov.opl.transition

import com.github.mbuzdalov.opl.MathEx.{logFactorialBig => lF}
import spire.implicits._

object BigDecimalProbabilityFinder extends TransitionProbabilityFinder {
  override def find(n: Int, d: Int, change: Int, target: Array[Double]): Unit = {
    val l = math.max(change / 2 + 1, change - n + d)
    val u = math.min(change, d)

    val common = lF(d) + lF(n - d) - lF(n) + lF(change) + lF(n - change)
    var k = 0
    while (k <= u - l) {
      val v = (common - lF(k + l) - lF(d - k - l) - lF(change - k - l) - lF(n - d - change + k + l)).exp()
      target(k) = v.toDouble
      k += 1
    }
  }
}
