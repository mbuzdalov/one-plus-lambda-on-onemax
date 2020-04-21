package com.github.mbuzdalov.opl.transition

import com.github.mbuzdalov.opl.MathEx.{logFactorialBig => lF}
import com.github.mbuzdalov.opl.DoubleProbabilityVector
import spire.implicits._

object BigDecimalProbabilityFinder extends TransitionProbabilityFinder {
  override def find(n: Int, d: Int, change: Int, target: DoubleProbabilityVector): Unit = {
    val l = math.max(change / 2 + 1, change - n + d)
    val u = math.min(change, d)
    target.setBounds(l, u)

    if (l <= u) {
      val common = lF(d) + lF(n - d) - lF(n) + lF(change) + lF(n - change)
      var k = l
      while (k <= u) {
        val v = (common - lF(k) - lF(d - k) - lF(change - k) - lF(n - d - change + k)).exp()
        target.setValue(k, v.toDouble)
        k += 1
      }
    }
  }
}
