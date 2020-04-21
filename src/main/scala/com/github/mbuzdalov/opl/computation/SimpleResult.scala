package com.github.mbuzdalov.opl.computation

class SimpleResult[@specialized P] private[computation] (val problemSize: Int,
                                                         val populationSize: Int,
                                                         expectations: Array[Double],
                                                         bestParameter: Array[P]) extends ComputationResult[P] {
  override def optimalParameter(distance: Int): P = bestParameter(distance - 1)
  override def optimalExpectation(distance: Int): Double = expectations(distance)
}
