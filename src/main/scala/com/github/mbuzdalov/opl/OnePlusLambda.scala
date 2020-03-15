package com.github.mbuzdalov.opl

class OnePlusLambda(n: Int, lambda: Int, listener: OnePlusLambdaListener) {
  private[this] val logChoose = new MathEx.LogChoose(n)
  private[this] val optimalTimeCache, driftMaximizingCache = Array.fill(n)(Double.NaN)
  private[this] val optimalByStrength, driftMaximizingByStrength, driftByStrength = Array.fill(n)(Double.NaN)

  computeEverything()

  private def multiplyByPower(power: Int, unit: ProbabilityVector, result: ProbabilityVector): Unit = {
    var p = power
    while (p > 1) {
      if ((p & 1) == 1) result *= unit
      unit *= unit
      p >>>= 1
    }
    if (p == 1) result *= unit
  }

  private def computeEverything(): Unit = {
    listener.startComputing(Seq(lambda))
    for (d <- 1 to n) {
      listener.startComputingDistance(lambda, d)
      var bestDriftValue = -1.0
      var bestDriftIndex = -1
      var minOptimal = Double.PositiveInfinity
      var minIndex = -1
      var change = 1
      while (change <= n) {
        compute(d, change)
        listener.distanceEllComputed(lambda, d, change,
                                     optimalByStrength(change - 1), driftByStrength(change - 1),
                                     driftMaximizingByStrength(change - 1))
        if (minOptimal > optimalByStrength(change - 1)) {
          minOptimal = optimalByStrength(change - 1)
          minIndex = change
        }
        minOptimal = math.min(minOptimal, optimalByStrength(change - 1))
        val driftValue = driftByStrength(change - 1)
        if (bestDriftValue < driftValue) {
          bestDriftValue = driftValue
          bestDriftIndex = change
        }
        change += 1
      }
      optimalTimeCache(d - 1) = minOptimal
      driftMaximizingCache(d - 1) = driftMaximizingByStrength(bestDriftIndex - 1)
      listener.finishComputingDistance(lambda, d, minOptimal, minIndex,
                                       driftMaximizingByStrength(bestDriftIndex - 1), bestDriftIndex, bestDriftValue)
    }
    val logAll = math.log(2) * n

    listener.summary(lambda = lambda,
                     expectedOptimal = (1 to n).map(d => optimalTimeCache(d - 1) * math.exp(logChoose(n, d) - logAll)).sum,
                     expectedDriftOptimal = (1 to n).map(d => driftMaximizingCache(d - 1) * math.exp(logChoose(n, d) - logAll)).sum)
    listener.finishComputing()
  }

  private def compute(d: Int, change: Int): Unit = {
    val lower = math.max((change + 1) / 2, change - n + d)
    val upper = math.min(change, d)
    val cnc = logChoose(n, change)
    val prob = new ProbabilityVector(upper - lower + 1)
    for (okay <- 0 to upper - lower) prob.set(okay, math.exp(logChoose(d, okay + lower) + logChoose(n - d, change - okay - lower) - cnc))
    prob.setPreDataByArray()
    if (prob.getPreData < 1) {
      multiplyByPower(lambda - 1, new ProbabilityVector(prob), prob)
    }

    var updateSumOptimal, updateSumDriftOptimal, drift = 0.0
    var updateProb = 0.0
    var i = 0
    while (i < prob.size) {
      val okay = lower + i
      val newD = d - 2 * okay + change
      val pi = prob.get(i)
      if (newD < d) {
        updateProb += pi
        if (newD > 0) {
          updateSumOptimal += optimalTimeCache(newD - 1) * pi
          updateSumDriftOptimal += driftMaximizingCache(newD - 1) * pi
        }
        drift += (d - newD) * pi
      }
      i += 1
    }

    optimalByStrength(change - 1) = (1 + updateSumOptimal) / updateProb
    driftMaximizingByStrength(change - 1) = (1 + updateSumDriftOptimal) / updateProb
    driftByStrength(change - 1) = drift
  }
}
