package com.github.mbuzdalov.opl.computation

import com.github.mbuzdalov.opl.distribution.FlipKBits
import com.github.mbuzdalov.opl.{DoubleProbabilityVector, TransitionMatrix}

object OptimalRunningTime {
  def newListener: ComputationListener[Int] = new SizedDelegatingListener[Int]((n, l) => new Delegate(n, l))

  class Result private[OptimalRunningTime] (val problemSize: Int,
                                            val populationSize: Int,
                                            expectations: Array[Double],
                                            bestBitFlip: Array[Int],
                                            bitFlipMatrix: Array[Array[Double]]) extends ComputationResult[Int] {
    override def optimalParameter(distance: Int): Int = bestBitFlip(distance - 1)
    override def optimalExpectation(distance: Int): Double = expectations(distance)
    override def optimalExpectationForParameter(distance: Int, parameter: Int): Double = bitFlipMatrix(distance - 1)(parameter - 1)
  }

  private class Delegate(problemSize: Int, populationSize: Int) extends ComputationListener[Int] {
    private[this] val expectations = new Array[Double](problemSize + 1)
    private[this] val bestBitFlip = new Array[Int](problemSize)
    private[this] val bitFlipMatrix = Array.fill(problemSize, problemSize)(0.0)

    private[this] val flipVector, distanceVector = new DoubleProbabilityVector(problemSize)

    override def startComputing(problemSize: Int, populationSize: Int): Unit =
      throw new IllegalStateException("Sizes are already set")

    override def processDistance(distance: Int, matrix: TransitionMatrix): Unit = {
      var change = 1
      var bestChange = 0
      var bestChangeValue = Double.PositiveInfinity
      while (change <= problemSize) {
        FlipKBits.initialize(problemSize, change, flipVector)
        distanceVector.setComposition(flipVector, matrix)
        distanceVector.raiseToPowerWithExcessOnSuffix(populationSize)
        val condExp = distanceVector.dotProduct(expectations)
        val totalProb = distanceVector.sum
        val changeExp = (1 + condExp) / totalProb
        if (bestChangeValue > changeExp) {
          bestChangeValue = changeExp
          bestChange = change
        }
        bitFlipMatrix(distance - 1)(change - 1) = changeExp
        change += 1
      }
      expectations(distance) = bestChangeValue
      bestBitFlip(distance - 1) = bestChange
    }

    override def toResult: ComputationResult[Int] =
      new Result(problemSize, populationSize, expectations, bestBitFlip, bitFlipMatrix)
  }
}
