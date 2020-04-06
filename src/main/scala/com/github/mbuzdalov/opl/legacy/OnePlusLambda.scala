package com.github.mbuzdalov.opl.legacy

import com.github.mbuzdalov.opl.MathEx.logChoose
import com.github.mbuzdalov.opl.transition.{BoundedProbabilityFinder, TransitionProbabilityFinder}

class OnePlusLambda(n: Int, lambda: Int, listener: OnePlusLambdaListener) {
  private[this] val optimalTimeCache, driftMaximizingCache = Array.fill(n)(Double.NaN)
  private[this] val driftMaximizingByStrength = Array.fill(n)(Double.NaN)
  private[this] val probabilityTarget = Array.fill(n + 1)(Double.NaN)
  private[this] val pv: TransitionProbabilityFinder = BoundedProbabilityFinder
  private[this] val finderAux = pv.newAuxiliaryData(n)

  computeEverything()

  private def computeEverything(): Unit = {
    listener.startComputing(n, lambda)
    for (d <- 1 to n) {
      listener.startComputingDistance(d)
      var bestDriftValue = -1.0
      var bestDriftIndex = -1
      var minOptimal = Double.PositiveInfinity
      var minIndex = -1
      var change = 1
      while (change <= n) {
        val (currMin, driftValue) = compute(d, change)
        if (minOptimal > currMin) {
          minOptimal = currMin
          minIndex = change
        }
        if (bestDriftValue < driftValue) {
          bestDriftValue = driftValue
          bestDriftIndex = change
        }
        change += 1
      }
      optimalTimeCache(d - 1) = minOptimal
      driftMaximizingCache(d - 1) = driftMaximizingByStrength(bestDriftIndex - 1)
      listener.finishComputingDistance(d, minOptimal, minIndex,
                                       driftMaximizingByStrength(bestDriftIndex - 1), bestDriftIndex, bestDriftValue)
    }
    val logAll = math.log(2) * n

    listener.finishComputing(
      expectedOptimal = (1 to n).map(d => optimalTimeCache(d - 1) * math.exp(logChoose(n, d) - logAll)).sum,
      expectedDriftOptimal = (1 to n).map(d => driftMaximizingCache(d - 1) * math.exp(logChoose(n, d) - logAll)).sum
    )
  }

  private def compute(d: Int, change: Int): (Double, Double) = {
    val lower = math.max((change + 1) / 2, change - n + d)
    val upper = math.min(change, d)
    var updateSumOptimal, updateSumDriftOptimal, drift = 0.0
    var updateProb = 0.0

    if (lower <= upper) {
      pv.find(n, lambda, d, change, probabilityTarget, finderAux)
      var okay = lower
      while (okay <= upper) {
        val newD = d - 2 * okay + change
        val pi = probabilityTarget(okay - lower)
        if (newD < d) {
          updateProb += pi
          if (newD > 0) {
            updateSumOptimal += optimalTimeCache(newD - 1) * pi
            updateSumDriftOptimal += driftMaximizingCache(newD - 1) * pi
          }
          drift += (d - newD) * pi
        }
        okay += 1
      }
    }

    driftMaximizingByStrength(change - 1) = (1 + updateSumDriftOptimal) / updateProb
    listener.distanceEllComputed(d, change,
                                 updateProb, 1 + updateSumOptimal,
                                 drift, 1 + updateSumDriftOptimal)

    ((1 + updateSumOptimal) / updateProb, drift)
  }
}
