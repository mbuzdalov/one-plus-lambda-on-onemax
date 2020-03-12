package com.github.mbuzdalov.opl

class OnePlusLambda(n: Int, lambda: Int) {
  private[this] val choose = {
    val result = new Array[Array[Double]](n + 1)
    var i = 0
    while (i <= n) {
      result(i) = new Array(i + 1)
      result(i)(0) = 1
      result(i)(i) = 1
      var j = 1
      while (j < i) {
        result(i)(j) = result(i - 1)(j - 1) + result(i - 1)(j)
        j += 1
      }
      i += 1
    }
    result
  }

  private def normalize(target: Array[Double]): Unit = {
    var sumT = 0.0
    var i = 0
    while (i < target.length) {
      sumT += target(i)
      i += 1
    }
    assert(sumT >= 0.999 && sumT <= 1.001, s"sum = $sumT")
    i = 0
    while (i < target.length) {
      target(i) /= sumT
      i += 1
    }
  }

  private def multiplyInPlace(a: Array[Double], b: Array[Double]): Unit = {
    var aa, bb = 0.0
    var i = 0
    while (i < a.length) {
      aa += a(i)
      bb += b(i)
      a(i) = aa * b(i) + bb * a(i) - a(i) * b(i)
      i += 1
    }
    normalize(a)
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

  def compute(dp: Array[Double], d: Int): Array[Double] = Array.tabulate(n + 1) { change =>
    if (change == 0) Double.PositiveInfinity else {
      val lower = math.max(0, change - n + d)
      val upper = math.min(change, d)
      val denom = 1 / choose(n)(change)
      val cd = choose(d)
      val cnd = choose(n - d)

      val prob = Array.tabulate(upper - lower + 1)(okay => cd(okay + lower) * cnd(change - okay - lower) * denom)
      assert(math.abs(prob.sum - 1) < 1e-9, s"prob.sum = ${prob.sum} at n=$n, d=$d, change=$change")
      multiplyByPower(lambda - 1, prob.clone(), prob)

      var updateSum = 0.0
      var updateProb, stayProb = 0.0
      var i = 0
      while (i < prob.length) {
        val okay = lower + i
        val newD = d - 2 * okay + change
        if (newD >= d) {
          stayProb += prob(i)
        } else {
          updateProb += prob(i)
          updateSum += dp(newD) * prob(i)
        }
        i += 1
      }

      assert(math.abs(updateProb + stayProb - 1) < 1e-9)

      if (stayProb > 1 - 1e-9)
        Double.PositiveInfinity
      else
        (1 + updateSum) / (1 - stayProb)
    }
  }
}

object OnePlusLambda {
  def main(args: Array[String]): Unit = {
    val n = args(0).toInt
    val lambda = args(1).toInt
    val ol = new OnePlusLambda(n, lambda)
    val dp = new Array[Double](n + 1)
    println(s"$n $lambda")
    for (d <- 1 to n) {
      val arr = ol.compute(dp, d)
      println(arr.mkString(s"$d: ", " ", ""))
      dp(d) = arr.min
    }
  }
}
