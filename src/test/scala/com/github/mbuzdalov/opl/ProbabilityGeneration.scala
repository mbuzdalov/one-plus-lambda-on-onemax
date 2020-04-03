package com.github.mbuzdalov.opl

import java.util.concurrent.ThreadLocalRandom

import scala.Ordering.Double.IeeeOrdering

import com.github.mbuzdalov.opl.transition.{BoundedProbabilityFinder, UnboundedProbabilityFinder}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProbabilityGeneration extends AnyFlatSpec with Matchers {
  def withBoundedProbabilityVector(n: Int, d: Int, change: Int, lambda: Int): Array[Double] = {
    val lower = math.max((change + 1) / 2, change - n + d)
    val upper = math.min(change, d)
    if (lower <= upper) {
      val target = new Array[Double](upper - lower + 1)
      BoundedProbabilityFinder.find(n, lambda, d, change, target)
      target
    } else {
      new Array(0)
    }
  }

  def withUnboundedProbabilityVector(n: Int, d: Int, change: Int, lambda: Int): Array[Double] = {
    val lower = math.max((change + 1) / 2, change - n + d)
    val upper = math.min(change, d)
    if (lower <= upper) {
      val target = new Array[Double](upper - lower + 1)
      UnboundedProbabilityFinder.find(n, lambda, d, change, target)
      target
    } else {
      new Array(0)
    }
  }

  def computeMaxDiff(a: Array[Double], b: Array[Double]): Double = a.indices.view.map(i => math.abs(a(i) - b(i))).max

  def validate(n: Int, d: Int, change: Int, lambda: Int): Unit = {
    val unbounded = withUnboundedProbabilityVector(n, d, change, lambda)
    val bounded = withBoundedProbabilityVector(n, d, change, lambda)
    assert(unbounded.length == bounded.length)
    if (unbounded.length > 0) {
      val maxDiff = computeMaxDiff(unbounded, bounded)
      assert(maxDiff <= 1e-9)
    }
  }

  def tortureTest(n: Int, lambda: Int): Unit = {
    val rng = ThreadLocalRandom.current()
    var count = 0
    while (count * n < 100000000) {
      val d = rng.nextInt(n) + 1
      val change = rng.nextInt(n) + 1
      validate(n, d, change, lambda)
      count += 1
    }
  }

  "all methods" should "produce the same results for n=100, d=30, change=3, lambda=1" in validate(100, 30, 3, 1)
  they should "produce the same results for n=100, d=30, change=3, lambda=10" in validate(100, 30, 3, 10)
  they should "produce the same results for n=100, d=5, change=2, lambda=1" in validate(100, 5, 2, 1)
  they should "produce the same results for n=100, d=5, change=2, lambda=10" in validate(100, 5, 2, 10)

  for (n <- Seq(10000, 100000); lambda <- Seq(1, 100, 10000))
    they should s"produce the same results for n=$n, d=10, change=3, lambda=$lambda" in validate(n, 10, 3, lambda)

  they should "produce the same results for n = 100, d = 23, change = 45, lambda = 1" in validate(n = 100, d = 23, change = 45, lambda = 1)
  they should "produce the same results for n = 10000, d = 3858, change = 2959, lambda = 1" in validate(n = 10000, d = 3858, change = 2959, lambda = 1)

  "the torture test" should "complete successfully" in {
    tortureTest(100, 1)
    tortureTest(100, 10)
    tortureTest(100, 100)
    tortureTest(1000, 1)
    tortureTest(1000, 10)
    tortureTest(1000, 100)
  }
}
