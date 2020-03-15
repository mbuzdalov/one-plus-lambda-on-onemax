package com.github.mbuzdalov.opl

class ProbabilityVector(val size: Int) {
  private[this] val data = new Array[Double](size)
  private[this] var preDataProbability = 0.0

  private def getData: Array[Double] = data

  def get(index: Int): Double = data(index)
  def set(index: Int, value: Double): Unit = data(index) = value

  def setPreDataByArray(): Unit = preDataProbability = 1 - data.sum

  def getPreData: Double = preDataProbability
  def setPreData(value: Double): Unit = preDataProbability = value

  def *= (that: ProbabilityVector): Unit = {
    val a = data
    val b = that.getData
    var aa = preDataProbability
    var bb = that.getPreData
    var sum = 0.0
    var i = 0
    while (i < a.length) {
      aa += a(i)
      bb += b(i)
      a(i) = aa * b(i) + bb * a(i) - a(i) * b(i)
      sum += a(i)
      i += 1
    }
    sum /= 1 - preDataProbability * that.getPreData
    i = 0
    while (i < a.length) {
      a(i) /= sum
      i += 1
    }
    preDataProbability *= that.getPreData
  }
}
