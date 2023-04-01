package com.github.mbuzdalov.oll

case class ExpectedWaitingTime(updateProbability: Double, conditionedExpectation: Double) {
  def toDouble: Double = conditionedExpectation / updateProbability

  def *(scenarioProbability: Double): ExpectedWaitingTime = {
    require(scenarioProbability <= 1, "Scenario probability is > 1")
    require(scenarioProbability >= 0, "Scenario probability must be positive")
    ExpectedWaitingTime(scenarioProbability * updateProbability, scenarioProbability * conditionedExpectation)
  }

  def +(that: ExpectedWaitingTime): ExpectedWaitingTime = {
    require(updateProbability + that.updateProbability <= 1, "Sum of probabilities is greater than 1")
    ExpectedWaitingTime(updateProbability + that.updateProbability, conditionedExpectation + that.conditionedExpectation)
  }
}
