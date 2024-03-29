package com.github.mbuzdalov.opl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OptimalStaticDistributionTests extends AnyFlatSpec with Matchers {
  "Optimal static distribution" should "be found correctly for n=10, lambda=1" in {
    val result = OptimalStaticDistribution.findOptimalDistribution(10, 1)
    result.fitness shouldBe (22.359033978174686 +- 1e-12)
    result.distribution(0) shouldBe (1.0 +- 1e-15)
    for (i <- 1 until 9) {
      result.distribution(i) shouldBe (0.0 +- 1e-15)
    }
  }

  it should "be found correctly for n=10, lambda=4" in {
    val result = (0 until 5).map{ _ =>
      OptimalStaticDistribution.findOptimalDistribution(10, 4)
    }.minBy(_.fitness)

    result.fitness shouldBe (8.06115492800143 +- 1e-12)
    result.distribution(0) shouldBe (0.9828011749325332 +- 2e-8)
    result.distribution(9) shouldBe (0.01719882506746691 +- 2e-8)
    for (i <- 1 until 8) {
      result.distribution(i) shouldBe (0.0 +- 1e-15)
    }
  }

  it should "be found correctly for n=20, lambda=256" in {
    val result = (0 until 10).map{ _ =>
      OptimalStaticDistribution.findOptimalDistribution(20, 256)
    }.minBy(_.fitness)

    result.fitness shouldBe (3.8274875626092526 +- 1e-12)
  }
}
