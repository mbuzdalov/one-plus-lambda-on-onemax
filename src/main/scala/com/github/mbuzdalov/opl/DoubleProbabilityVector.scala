package com.github.mbuzdalov.opl

import java.util.{Arrays => JArrays}

class DoubleProbabilityVector(n: Int) extends ProbabilityVector {
  private[this] val probabilities = new Array[Double](n + 1)
  private[this] var lower, upper: Int = _

  def setBounds(lower: Int, upper: Int): Unit = {
    this.lower = lower
    this.upper = upper
  }

  def setValue(distance: Int, probability: Double): Unit = {
    assert(lower <= distance && distance <= upper)
    probabilities(distance) = probability
  }

  override def smallestDistance: Int = lower
  override def largestDistance: Int = upper
  override def probabilityAsDouble(distance: Int): Double = probability(distance)

  def probability(distance: Int): Double = {
    require(lower <= distance && distance <= upper)
    probabilities(distance)
  }

  def sum: Double = {
    var i = lower
    var result = 0.0
    while (i <= upper) {
      result += probabilities(i)
      i += 1
    }
    result
  }

  def dotProduct(that: Array[Double]): Double = {
    var i = lower
    var result = 0.0
    while (i <= upper) {
      result += that(i) * probabilities(i)
      i += 1
    }
    result
  }

  def raiseToPowerWithExcessOnSuffix(power: Int): Unit = {
    require(power >= 1)
    if (power > 1) {
      var sumSuffix = 1.0 - sum
      var i = upper
      while (i >= lower) {
        val nextSum = sumSuffix + probabilities(i)
        probabilities(i) = math.pow(nextSum, power) - math.pow(sumSuffix, power)
        sumSuffix = nextSum
        i -= 1
      }
    }
  }

  def setComposition(first: DoubleProbabilityVector, second: TransitionMatrix): Unit = {
    val firstLower = first.smallestDistance
    val firstUpper = first.largestDistance
    var firstIndex = firstLower
    var firstTime = true
    while (firstIndex <= firstUpper) {
      val prob = first.probability(firstIndex)
      val secondLower = second.minDistance(firstIndex)
      val secondUpper = second.maxDistance(firstIndex)
      val secondStep = second.stepDistance(firstIndex)

      if (firstTime) {
        firstTime = false
        setBounds(secondLower, secondUpper)
        JArrays.fill(probabilities, secondLower, secondUpper + 1, 0.0)
      } else {
        if (lower > secondLower) {
          JArrays.fill(probabilities, secondLower, lower, 0.0)
          lower = secondLower
        }
        if (upper < secondUpper) {
          JArrays.fill(probabilities, upper + 1, secondUpper + 1, 0.0)
          upper = secondUpper
        }
      }

      var secondIndex = secondLower
      while (secondIndex <= secondUpper) {
        probabilities(secondIndex) += prob * second.probability(firstIndex, secondIndex)
        secondIndex += secondStep
      }

      firstIndex += 1
    }
  }
}
