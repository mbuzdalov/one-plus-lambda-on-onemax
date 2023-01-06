package com.github.mbuzdalov.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MathExTests extends AnyFlatSpec with Matchers {
  "next power of two" should "be 1 for 1" in {MathEx.nextPowerOfTwo(1) shouldBe 1}
  it should "be 2 for 2" in {MathEx.nextPowerOfTwo(2) shouldBe 2}
  for (i <- 2 to 30; p = 1 << i) {
    it should s"be $p for $p" in {MathEx.nextPowerOfTwo(p) shouldBe p}
    it should s"be $p for ${p - 1}" in {MathEx.nextPowerOfTwo(p - 1) shouldBe p}
    if (p > 4) {
      it should s"be $p for ${p / 2 + 1}" in {MathEx.nextPowerOfTwo(p / 2 + 1) shouldBe p}
    }
  }

  "MathEx.logChoose" should "compute choose(0,0) correctly" in {MathEx.logChoose(0, 0) shouldBe 0.0}
  it should "compute choose(1,1) correctly" in {MathEx.logChoose(1, 1) shouldBe 0.0}
  it should "compute choose(4,2) correctly" in {MathEx.logChoose(4, 2) should be (math.log(6) +- 1e-12)}

  "MathEx.logFactorialBig" should "compute 5! correctly" in {MathEx.logFactorialBig(5).toDouble should be (math.log(120) +- 1e-12)}
}
