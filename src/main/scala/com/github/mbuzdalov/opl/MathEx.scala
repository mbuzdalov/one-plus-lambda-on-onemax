package com.github.mbuzdalov.opl

object MathEx {
  final class LogChoose(maxN: Int) {
    def apply(n: Int, k: Int): Double = logFactorial(n) - logFactorial(n - k) - logFactorial(k)

    private[this] val logFactorial = {
      val result = new Array[Double](maxN + 1)
      var i = 2
      while (i <= maxN) {
        result(i) = result(i - 1) + math.log(i)
        i += 1
      }
      result
    }
  }
}
