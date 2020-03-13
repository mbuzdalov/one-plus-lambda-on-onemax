package com.github.mbuzdalov.opl

import scala.Ordering.Double.IeeeOrdering

class OnePlusLambda(n: Int, lambda: Int) {
  private[this] val logChoose = new MathEx.LogChoose(n)
  private[this] val optimalTimeCache, driftMaximizingCache = Array.fill(n)(Double.NaN)
  private[this] val optimalByStrength, driftMaximizingByStrength, driftByStrength = Array.fill(n, n)(Double.NaN)

  private def multiplyInPlace(a: Array[Double], b: Array[Double]): Unit = {
    var aa, bb, sum = 0.0
    var i = 0
    while (i < a.length) {
      aa += a(i)
      bb += b(i)
      a(i) = aa * b(i) + bb * a(i) - a(i) * b(i)
      sum += a(i)
      i += 1
    }
    i = 0
    while (i < a.length) {
      a(i) /= sum
      i += 1
    }
  }

  private def multiplyByPower(power: Int, unit: Array[Double], result: Array[Double]): Unit = {
    var p = power
    while (p > 1) {
      if ((p & 1) == 1) multiplyInPlace(result, unit)
      multiplyInPlace(unit, unit)
      p >>>= 1
    }
    if (p == 1) multiplyInPlace(result, unit)
  }

  def getOptimalTime(d: Int): Double = {
    if (d == 0)
      0.0
    else {
      if (optimalTimeCache(d - 1).isNaN)
        optimalTimeCache(d - 1) = (1 to n).view.map(l => getOptimalTime(d, l)).min
      optimalTimeCache(d - 1)
    }
  }

  def getDriftOptimalTime(d: Int): Double = {
    if (d == 0)
      0.0
    else {
      if (driftMaximizingCache(d - 1).isNaN) {
        val bestDrift = (1 to n).maxBy(l => getDrift(d, l))
        driftMaximizingCache(d - 1) = getDriftOptimalTime(d, bestDrift)
      }
      driftMaximizingCache(d - 1)
    }
  }

  def getOptimalTime(d: Int, l: Int): Double = {
    if (d == 0)
      0.0
    else if (l == 0)
      Double.PositiveInfinity
    else {
      if (optimalByStrength(d - 1)(l - 1).isNaN) compute(d, l)
      optimalByStrength(d - 1)(l - 1)
    }
  }

  def getDrift(d: Int, l: Int): Double = {
    if (d == 0 || l == 0)
      0.0
    else {
      if (driftByStrength(d - 1)(l - 1).isNaN) compute(d, l)
      driftByStrength(d - 1)(l - 1)
    }
  }

  def getDriftOptimalTime(d: Int, l: Int): Double = {
    if (d == 0)
      0.0
    else if (l == 0)
      Double.PositiveInfinity
    else {
      if (driftMaximizingByStrength(d - 1)(l - 1).isNaN) compute(d, l)
      driftMaximizingByStrength(d - 1)(l - 1)
    }
  }

  private def compute(d: Int, change: Int): Unit = {
    val lower = math.max(0, change - n + d)
    val upper = math.min(change, d)
    val cnc = logChoose(n, change)
    val prob = Array.tabulate(upper - lower + 1)(okay => math.exp(logChoose(d, okay + lower) + logChoose(n - d, change - okay - lower) - cnc))
    assert(math.abs(prob.sum - 1) < 1e-9, s"prob.sum = ${prob.sum} at n=$n, d=$d, change=$change")
    multiplyByPower(lambda - 1, prob.clone(), prob)

    var updateSumOptimal, updateSumDriftOptimal, drift = 0.0
    var updateProb, stayProb = 0.0
    var i = 0
    while (i < prob.length) {
      val okay = lower + i
      val newD = d - 2 * okay + change
      val pi = prob(i)
      if (newD >= d) {
        stayProb += pi
      } else {
        updateProb += pi
        updateSumOptimal += getOptimalTime(newD) * pi
        updateSumDriftOptimal += getDriftOptimalTime(newD) * pi
        drift += (d - newD) * pi
      }
      i += 1
    }

    assert(math.abs(updateProb + stayProb - 1) < 1e-9)

    if (stayProb > 1 - 1e-9) {
      optimalByStrength(d - 1)(change - 1) = Double.PositiveInfinity
      driftMaximizingByStrength(d - 1)(change - 1) = Double.PositiveInfinity
      driftByStrength(d - 1)(change - 1) = 0
    } else {
      optimalByStrength(d - 1)(change - 1) = (1 + updateSumOptimal) / (1 - stayProb)
      driftMaximizingByStrength(d - 1)(change - 1) = (1 + updateSumDriftOptimal) / (1 - stayProb)
      driftByStrength(d - 1)(change - 1) = drift
    }
  }
}
