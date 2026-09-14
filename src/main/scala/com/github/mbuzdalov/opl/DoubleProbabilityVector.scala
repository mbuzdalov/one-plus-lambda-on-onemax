package com.github.mbuzdalov.opl

import com.github.mbuzdalov.util.Loops.{loopFromDownTo, loopFromTo}

import java.util.Arrays as JArrays
import scala.compiletime.uninitialized

class DoubleProbabilityVector(n: Int) extends ProbabilityVector:
  private val probabilities = Array.ofDim[Double](n + 1)
  private var lower, upper: Int = uninitialized

  def setBounds(lower: Int, upper: Int): Unit =
    this.lower = lower
    this.upper = upper

  def setValue(distance: Int, probability: Double): Unit =
    assert(lower <= distance && distance <= upper)
    probabilities(distance) = probability

  override def smallestDistance: Int = lower
  override def largestDistance: Int = upper
  override def probabilityAsDouble(distance: Int): Double = probability(distance)

  def probability(distance: Int): Double =
    require(lower <= distance && distance <= upper)
    probabilities(distance)

  def sum: Double =
    var result = 0.0
    loopFromTo(lower, upper): i =>
      result += probabilities(i)
    result

  def dotProduct(that: Array[Double]): Double =
    var result = 0.0
    loopFromTo(lower, upper): i =>
      result += that(i) * probabilities(i)
    result

  def raiseToPowerWithExcessOnSuffix(power: Int): Unit =
    require(power >= 1)
    if power > 1 then
      var sumSuffix = 1.0 - sum
      loopFromDownTo(upper, lower): i =>
        val nextSum = sumSuffix + probabilities(i)
        probabilities(i) = math.pow(nextSum, power) - math.pow(sumSuffix, power)
        sumSuffix = nextSum
      compact()

  private def compact(): Unit =
    while lower <= upper && probabilities(lower) == 0 do lower += 1
    while lower <= upper && probabilities(upper) == 0 do upper -= 1

  def setComposition(first: DoubleProbabilityVector, second: TransitionMatrix): Unit =
    val firstLower = first.smallestDistance
    val firstUpper = first.largestDistance
    var firstTime = true
    loopFromTo(firstLower, firstUpper): firstIndex =>
      val prob = first.probability(firstIndex)
      if prob > 0 then
        val secondLower = second.minDistance(firstIndex)
        val secondUpper = second.maxDistance(firstIndex)
        val secondStep = second.stepDistance(firstIndex)

        if secondLower <= secondUpper then
          if firstTime then
            firstTime = false
            setBounds(secondLower, secondUpper)
            JArrays.fill(probabilities, secondLower, secondUpper + 1, 0.0)
          else
            if lower > secondLower then
              JArrays.fill(probabilities, secondLower, lower, 0.0)
              lower = secondLower
            if upper < secondUpper then
              JArrays.fill(probabilities, upper + 1, secondUpper + 1, 0.0)
              upper = secondUpper

          var secondIndex = secondLower
          while secondIndex <= secondUpper do
            probabilities(secondIndex) += prob * second.probability(firstIndex, secondIndex)
            secondIndex += secondStep

    if firstTime then setBounds(1, 0)
    compact()
