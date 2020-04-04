package com.github.mbuzdalov.opl.transition

object ProbabilityVectorUtils {
  def multiplyInPlace(a: Array[Double], b: Array[Double]): Unit = {
    var aa, bb = 0.0
    var i = 0
    while (i < a.length) {
      aa += a(i)
      bb += b(i)
      a(i) = aa * b(i) + bb * a(i) - a(i) * b(i)
      i += 1
    }
  }

  def multiplyByPower(power: Int, unit: Array[Double], result: Array[Double]): Unit = {
    var p = power
    while (p > 1) {
      if ((p & 1) == 1) multiplyInPlace(result, unit)
      multiplyInPlace(unit, unit)
      p >>>= 1
    }
    if (p == 1) multiplyInPlace(result, unit)
  }
}
