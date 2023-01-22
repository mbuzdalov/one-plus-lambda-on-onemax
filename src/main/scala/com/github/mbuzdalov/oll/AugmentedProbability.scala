package com.github.mbuzdalov.oll

case class AugmentedProbability(value: Double) {
  lazy val pOverOneMinusP: Double = value / (1 - value)
  lazy val logarithm: Double = math.log(value)
  lazy val logarithmOfOneMinus: Double = math.log1p(-value)
}
