package com.github.mbuzdalov.opl

import com.github.mbuzdalov.opl.MathEx.LogChoose
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MathExTests extends AnyFlatSpec with Matchers {
  private val logChoose = new LogChoose(10)

  "LogChoose" should "compute choose(0,0) correctly" in {logChoose(0, 0) shouldBe 0.0}
  it should "compute choose(1,1) correctly" in {logChoose(1, 1) shouldBe 0.0}
  it should "compute choose(4,2) correctly" in {logChoose(4, 2) should be (math.log(6) +- 1e-12)}
}
