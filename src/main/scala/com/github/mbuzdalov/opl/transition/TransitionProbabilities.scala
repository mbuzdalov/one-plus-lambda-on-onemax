package com.github.mbuzdalov.opl.transition

class TransitionProbabilities(n: Int) {
  private[this] val probabilities = new Array[Double](n + 1)
  private[this] var lower, upper: Int = _

  private[transition] def setBounds(lower: Int, upper: Int): Unit = {
    this.lower = lower
    this.upper = upper
  }

  private[transition] def setValue(distance: Int, probability: Double): Unit = {
    assert(lower <= distance && distance <= upper)
    probabilities(distance) = probability
  }

  def smallestDistance: Int = lower
  def largestDistance: Int = upper
  def isEmpty: Boolean = lower > upper
  def nonEmpty: Boolean = lower <= upper

  def probability(distance: Int): Double = {
    require(lower <= distance && distance <= upper)
    probabilities(distance)
  }
}
