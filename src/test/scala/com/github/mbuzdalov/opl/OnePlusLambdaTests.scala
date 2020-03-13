package com.github.mbuzdalov.opl

import com.github.mbuzdalov.opl.MathEx.LogChoose
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OnePlusLambdaTests extends AnyFlatSpec with Matchers {
  private def optimalTimes(n: Int): (Double, Double) = {
    val logAll = math.log(2) * n
    val opl = new OnePlusLambda(n, 1)
    val logChoose = new LogChoose(n)
    val optimalTime = (0 to n).map(d => opl.getOptimalTime(d) * math.exp(logChoose(n, d) - logAll)).sum
    val driftOptimalTime = (0 to n).map(d => opl.getDriftOptimalTime(d) * math.exp(logChoose(n, d) - logAll)).sum
    (optimalTime, driftOptimalTime)
  }

  "RLS time" should "be right for n=500" in {
    val (optimalTime, driftOptimalTime) = optimalTimes(500)
    optimalTime should be (2974.0 +- 0.05)
    driftOptimalTime should be (2974.3 +- 0.05)
  }

  it should "be right for n=1000" in {
    val (optimalTime, driftOptimalTime) = optimalTimes(1000)
    optimalTime should be (6644.0 +- 0.05)
    driftOptimalTime should be (6644.2 +- 0.05)
  }

  it should "be right for n=1500" in {
    val (optimalTime, driftOptimalTime) = optimalTimes(1500)
    optimalTime should be (10575.7 +- 0.05)
    driftOptimalTime should be (10575.9 +- 0.05)
  }
}
