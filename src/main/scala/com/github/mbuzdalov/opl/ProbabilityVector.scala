package com.github.mbuzdalov.opl

class ProbabilityVector(val size: Int) {
  private[this] val data = new Array[Double](size + 1)
  data(0) = 1.0

  private def getData: Array[Double] = data

  def get(index: Int): Double = data(index + 1)
  def set(index: Int, value: Double): Unit = data(index + 1) = value

  def setPreDataByArray(): Unit = {
    var preDataProbability = 1.0
    var i = data.length
    while (i > 1) {
      i -= 1
      preDataProbability -= data(i)
    }
    data(0) = preDataProbability
  }

  def getPreData: Double = data(0)
  def setPreData(value: Double): Unit = data(0) = value

  def *= (that: ProbabilityVector): Unit = ProbabilityVector.multiplyInPlace(data, that.getData)
}

object ProbabilityVector {
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

  def multiplyByPower(power: Int, unit: ProbabilityVector, result: ProbabilityVector): Unit = {
    var p = power
    while (p > 1) {
      if ((p & 1) == 1) result *= unit
      unit *= unit
      p >>>= 1
    }
    if (p == 1) result *= unit
  }
}
