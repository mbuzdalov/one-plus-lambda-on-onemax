package com.github.mbuzdalov.opl

trait ProbabilityVector {
  def smallestDistance: Int
  def largestDistance: Int
  def isEmpty: Boolean = smallestDistance > largestDistance
  def nonEmpty: Boolean = smallestDistance <= largestDistance

  def probabilityAsDouble(distance: Int): Double
}
