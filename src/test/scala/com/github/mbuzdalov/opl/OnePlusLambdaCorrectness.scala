package com.github.mbuzdalov.opl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OnePlusLambdaCorrectness extends AnyFlatSpec with Matchers {
  "The OnePlusLambda class" should "successfully create and do not crash" in {
    val opl = new OnePlusLambda(100, 5)
    val dist = new Array[Double](100)
    opl.compute(dist, 1)
  }
}
