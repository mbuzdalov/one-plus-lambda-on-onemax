package com.github.mbuzdalov.opl.computation

import com.github.mbuzdalov.util.MathEx

trait BareComputationResult {
  def problemSize: Int
  def populationSize: Int
  def optimalExpectation(distance: Int): Double

  lazy val expectedRunningTime: Double = MathEx.expectedRuntimeOnBitStrings(problemSize, optimalExpectation)
}
