package com.github.mbuzdalov.opl.computation

import scala.Ordering.Double.IeeeOrdering
import scala.annotation.tailrec

object OptimalMutationRunningTime {
  def newListener(useShift: Boolean): ComputationListener[Double] =
    new SizedDelegatingListener[Double]((n, l) => new Delegate(n, l, useShift))

  class Result private[OptimalMutationRunningTime] (val problemSize: Int,
                                                    val populationSize: Int,
                                                    expectations: Array[Double],
                                                    bestMutationProbability: Array[Double],
                                                    conditionalExpectations: Array[Array[Double]],
                                                    updateProbabilities: Array[Array[Double]],
                                                    useShift: Boolean) extends ComputationResult[Double] {
    override def optimalParameter(distance: Int): Double = bestMutationProbability(distance - 1)
    override def optimalExpectation(distance: Int): Double = expectations(distance - 1)
    override def optimalExpectationForBitFlips(distance: Int, flips: Int): Double =
      (1 + conditionalExpectations(distance - 1)(flips - 1)) / updateProbabilities(distance - 1)(flips - 1)
    override def optimalExpectationForParameter(distance: Int, parameter: Double): Double =
      computeExpectedRunningTime(conditionalExpectations(distance - 1), updateProbabilities(distance - 1),
                                 parameter, useShift)
  }

  private class Delegate(problemSize: Int, populationSize: Int, useShift: Boolean) extends ComputationListener[Double] {
    private[this] val expectations = new Array[Double](problemSize)
    private[this] val bestMutationProbability = new Array[Double](problemSize)
    private[this] val conditionalExpectations, updateProbabilities = Array.fill(problemSize, problemSize)(0.0)
    private[this] val tracker = new ConditionalExpectationTracker(expectations)

    override def startComputing(problemSize: Int, populationSize: Int): Unit =
      throw new IllegalStateException("Sizes are already set")

    override def startDistance(distance: Int): Unit = {}

    override def startTransitionProbabilityGroup(distance: Int, change: Int): Unit =
      tracker.reset()

    override def receiveTransitionProbability(change: Int, currDistance: Int, newDistance: Int, probability: Double): Unit =
      tracker.receiveProbability(newDistance, probability)

    override def finishTransitionProbabilityGroup(distance: Int, change: Int): Unit = {
      conditionalExpectations(distance - 1)(change - 1) = tracker.getConditionalExpectation
      updateProbabilities(distance - 1)(change - 1) = tracker.getUpdateProbability
    }

    override def finishDistance(distance: Int): Unit = {
      val condExp = conditionalExpectations(distance - 1)
      val updProb = updateProbabilities(distance - 1)
      val bestMutation = findBestMutationProbability(condExp, updProb, useShift)
      bestMutationProbability(distance - 1) = bestMutation
      expectations(distance - 1) = computeExpectedRunningTime(condExp, updProb, bestMutation, useShift)
      assert(expectations(distance - 1).isFinite)
    }

    override def toResult: ComputationResult[Double] =
      new Result(problemSize, populationSize, expectations, bestMutationProbability,
                 conditionalExpectations, updateProbabilities, useShift)
  }

  private def computeExpectedRunningTime(conditionalExpectations: Array[Double], updateProbabilities: Array[Double],
                                         probability: Double, useShift: Boolean): Double = {
    if (probability == 0) {
      // Flip nothing. This depends on useShift
      if (useShift)
        (1 + conditionalExpectations(0)) / updateProbabilities(0)
      else
        Double.PositiveInfinity
    } else if (probability == 1) {
      // A complete inversion. useShift does not matter
      (1 + conditionalExpectations.last) / updateProbabilities.last
    } else {
      // P[t bits flipped] = p^t * (1-p)^(n-t) * choose(n, t)
      import com.github.mbuzdalov.opl.MathEx.{logFactorial => lF}
      val logP = math.log(probability) // now those would not be infinities
      val log1P = math.log1p(-probability)

      var totalConditionalExpectation, totalUpdateProbability = 0.0
      var nFlipped = 0
      val n = conditionalExpectations.length
      val lfn = lF(n)
      while (nFlipped < n) {
        val ce = conditionalExpectations(nFlipped)
        val up = updateProbabilities(nFlipped)
        nFlipped += 1
        // ce can be Infinity, as one cannot reach an improvement from that point (e.g. distance=1, bit flips=2)
        // We shall not count that as an update!
        // In the case of an FP overflow, this seems to be also safe unless we to numeric search
        if (ce.isFinite) {
          val prob = math.exp(logP * nFlipped + log1P * (n - nFlipped) + lfn - lF(nFlipped) - lF(n - nFlipped))
          totalUpdateProbability += up * prob
          totalConditionalExpectation += ce * prob
        }
      }
      if (useShift) {
        val ce = conditionalExpectations(0)
        val up = updateProbabilities(0)
        if (ce.isFinite) {
          val prob = math.exp(log1P * n)
          totalUpdateProbability += up * prob
          totalConditionalExpectation += ce * prob
        }
      }
      val rv = (1 + totalConditionalExpectation) / totalUpdateProbability
      assert(!rv.isNaN)
      rv
    }
  }

  @tailrec
  private def ternarySearch(conditionalExpectations: Array[Double],
                            updateProbabilities: Array[Double],
                            useShift: Boolean, left: Double, right: Double, remainingIterations: Int): Double = {
    if (remainingIterations == 0)
      (left + right) / 2
    else {
      val ll = (left * 2 + right) / 3
      val rr = (left + 2 * right) / 3
      val lv = computeExpectedRunningTime(conditionalExpectations, updateProbabilities, ll, useShift)
      val rv = computeExpectedRunningTime(conditionalExpectations, updateProbabilities, rr, useShift)
      if (lv <= rv || rv.isInfinity) // this would decrease the probability if everything does to infinity
        ternarySearch(conditionalExpectations, updateProbabilities, useShift, left, rr, remainingIterations - 1)
      else
        ternarySearch(conditionalExpectations, updateProbabilities, useShift, ll, right, remainingIterations - 1)
    }
  }

  private def findBestMutationProbability(conditionalExpectations: Array[Double],
                                          updateProbabilities: Array[Double],
                                          useShift: Boolean): Double = {
    if (useShift) {
      val n = conditionalExpectations.length
      val probLarge = ternarySearch(conditionalExpectations, updateProbabilities, useShift, 1.0 / n, 1.0, 75)
      val valueLarge = computeExpectedRunningTime(conditionalExpectations, updateProbabilities, probLarge, useShift)
      val probSmall = ternarySearch(conditionalExpectations, updateProbabilities, useShift, 0, 1.0 / n, 75)
      val valueSmall = computeExpectedRunningTime(conditionalExpectations, updateProbabilities, probSmall, useShift)
      val value0 = computeExpectedRunningTime(conditionalExpectations, updateProbabilities, 0, useShift)
      val value1 = computeExpectedRunningTime(conditionalExpectations, updateProbabilities, 1, useShift)
      Array((probLarge, valueLarge), (probSmall, valueSmall), (0.0, value0), (1.0, value1)).minBy(_._2)._1
    } else {
      val value1 = computeExpectedRunningTime(conditionalExpectations, updateProbabilities, 1, useShift)
      val ternary = ternarySearch(conditionalExpectations, updateProbabilities, useShift, 0, 1.0, 75)
      val ternaryV = computeExpectedRunningTime(conditionalExpectations, updateProbabilities, ternary, useShift)
      if (value1 < ternaryV)
        1
      else
        ternary
    }
  }
}
