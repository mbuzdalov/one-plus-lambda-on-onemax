package com.github.mbuzdalov.opl

class SummaryOnlyListener extends OnePlusLambdaListener.Adapter {
  private[this] var optimalTime, driftOptimalTime = -1.0

  override def summary(lambda: Int, expectedOptimal: Double, expectedDriftOptimal: Double): Unit = {
    this.optimalTime = expectedOptimal
    this.driftOptimalTime = expectedDriftOptimal
  }

  def expectedOptimalTime: Double = optimalTime
  def expectedDriftOptimalTime: Double = driftOptimalTime
}
