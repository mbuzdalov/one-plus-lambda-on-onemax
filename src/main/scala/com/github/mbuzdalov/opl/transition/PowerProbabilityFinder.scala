package com.github.mbuzdalov.opl.transition

import com.github.mbuzdalov.opl.MathEx.{logFactorial => lF}

object PowerProbabilityFinder extends TransitionProbabilityFinder {
  override def find(n: Int, lambda: Int, d: Int, change: Int, target: Array[Double]): Unit = {
    val l = math.max((change + 1) / 2, change - n + d)
    val u = math.min(change, d)

    val common = lF(d) + lF(n - d) - lF(n) + lF(change) + lF(n - change)
    var k = 0
    var sum = 0.0
    while (k <= u - l) {
      val v = math.exp(common - lF(k + l) - lF(d - k - l) - lF(change - k - l) - lF(n - d - change + k + l))
      sum += v
      target(k) = v
      k += 1
    }

    sum = 1 - sum
    var lastPow = math.pow(sum, lambda)
    k = 0
    while (k <= u - l) {
      val v = target(k)
      sum += v
      val currPow = math.pow(sum, lambda)
      target(k) = currPow - lastPow
      lastPow = currPow
      k += 1
    }
  }
}
