package com.github.mbuzdalov.opl.computation

private class BitFlipMatrixFiller(expectations: Array[Double], bitFlipMatrix: Array[Array[Double]]) {
  private[this] var currDistance, currFlips: Int = _
  private[this] var updateProbability, conditionedExpectation: Double = _

  final def startDistance(distance: Int): Unit =
    currDistance = distance

  final def startGroup(change: Int): Unit = {
    currFlips = change
    updateProbability = 0
    conditionedExpectation = 0
  }

  def receiveProbability(newDistance: Int, probability: Double): Unit = {
    assert(newDistance < currDistance)
    updateProbability += probability
    if (newDistance >= 1)
      conditionedExpectation += probability * expectations(newDistance - 1)
  }

  def finishGroup(): Unit =
    bitFlipMatrix(currDistance - 1)(currFlips - 1) = (1 + conditionedExpectation) / updateProbability
}
