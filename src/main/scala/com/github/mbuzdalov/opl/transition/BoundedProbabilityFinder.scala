package com.github.mbuzdalov.opl.transition

import com.github.mbuzdalov.opl.MathEx.{logFactorial => lF}
import com.github.mbuzdalov.opl.ProbabilityVector

object BoundedProbabilityFinder extends TransitionProbabilityFinder {
  override def find(n: Int, lambda: Int, d: Int, change: Int, target: Array[Double]): Unit = {
    val l = math.max((change + 1) / 2, change - n + d)
    val u = math.min(change, d)

    val common = lF(d) + lF(n - d) - lF(n) + lF(change) + lF(n - change)
    val unit, prob = new ProbabilityVector(u - l + 1)
    var k = 0
    while (k <= u - l) {
      val dv = math.exp(common - lF(k + l) - lF(d - k - l) - lF(change - k - l) - lF(n - d - change + k + l))
      unit.set(k, dv)
      k += 1
    }

    unit.setPreDataByArray()
    if (unit.getPreData < 1)
      ProbabilityVector.multiplyByPower(lambda, unit, prob)

    var i = u - l
    while (i >= 0) {
      target(i) = prob.get(i)
      i -= 1
    }
  }
}
