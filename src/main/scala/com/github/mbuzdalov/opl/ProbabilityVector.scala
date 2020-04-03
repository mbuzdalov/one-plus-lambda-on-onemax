package com.github.mbuzdalov.opl

class ProbabilityVector(val size: Int) {
  private[this] val data = new Array[Double](size)
  private[this] var preDataProbability = 1.0

  private def getData: Array[Double] = data

  def get(index: Int): Double = data(index)
  def set(index: Int, value: Double): Unit = data(index) = value

  def setPreDataByArray(): Unit = {
    preDataProbability = 1.0
    var i = data.length
    while (i > 0) {
      i -= 1
      preDataProbability -= data(i)
    }
  }

  def getPreData: Double = preDataProbability
  def setPreData(value: Double): Unit = preDataProbability = value

  def *= (that: ProbabilityVector): Unit = {
    val a = data
    val b = that.getData
    var aa = preDataProbability
    var bb = that.getPreData
    var i = 0
    while (i < a.length) {
      aa += a(i)
      bb += b(i)
      a(i) = aa * b(i) + bb * a(i) - a(i) * b(i)
      i += 1
    }
    preDataProbability *= that.getPreData
  }
}

object ProbabilityVector {
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
