package com.github.mbuzdalov.opl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OnePlusLambdaTests extends AnyFlatSpec with Matchers {
  private def optimalTimes(n: Int, lambda: Int): (Double, Double) = {
    val opl = new OnePlusLambda(n, lambda, OnePlusLambdaListener.Idle)
    val optimalTime = opl.optimalExpectedTime
    val driftOptimalTime = opl.driftOptimalExpectedTime
    (optimalTime, driftOptimalTime)
  }

  "RLS time" should "be right for n=500" in {
    val (optimalTime, driftOptimalTime) = optimalTimes(500, 1)
    optimalTime should be (2974.0 +- 0.05)
    driftOptimalTime should be (2974.3 +- 0.05)
  }

  it should "be right for n=1000" in {
    val (optimalTime, driftOptimalTime) = optimalTimes(1000, 1)
    optimalTime should be (6644.0 +- 0.05)
    driftOptimalTime should be (6644.2 +- 0.05)
  }

  it should "be right for n=1500" in {
    val (optimalTime, driftOptimalTime) = optimalTimes(1500, 1)
    optimalTime should be (10575.7 +- 0.05)
    driftOptimalTime should be (10575.9 +- 0.05)
  }

  "(1+100) RLS time" should "be right for n=500" in {
    val (optimalTime, driftOptimalTime) = optimalTimes(500, 100)
    optimalTime should be (119.93759945813207 +- 1e-11)
    driftOptimalTime should be (119.94149522047802 +- 1e-11)
  }
}
