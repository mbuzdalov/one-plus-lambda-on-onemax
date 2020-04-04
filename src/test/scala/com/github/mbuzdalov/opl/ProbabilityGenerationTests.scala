package com.github.mbuzdalov.opl

import java.util.concurrent.ThreadLocalRandom

import scala.Ordering.Double.IeeeOrdering

import com.github.mbuzdalov.opl.transition.{BigDecimalBoundedProbabilityFinder, BoundedProbabilityFinder, TransitionProbabilityFinder, UnboundedProbabilityFinder}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProbabilityGenerationTests extends AnyFlatSpec with Matchers {
  private[this] val scaledEpsilon = 1e-14 // 6e-15 does not work

  def evaluate(n: Int, d: Int, change: Int, lambda: Int, finder: TransitionProbabilityFinder): Array[Double] = {
    val lower = math.max((change + 1) / 2, change - n + d)
    val upper = math.min(change, d)
    if (lower <= upper) {
      val target = new Array[Double](upper - lower + 1)
      finder.find(n, lambda, d, change, target)
      target
    } else {
      new Array(0)
    }
  }

  def computeMaxDiff(a: Array[Double], b: Array[Double]): Double = a.indices.view.map(i => math.abs(a(i) - b(i))).max

  def validate(n: Int, d: Int, change: Int, lambda: Int): Unit = {
    val unbounded = evaluate(n, d, change, lambda, UnboundedProbabilityFinder)
    val bounded = evaluate(n, d, change, lambda, BoundedProbabilityFinder)
    val epsilon = scaledEpsilon * n
    assert(unbounded.length == bounded.length)
    if (unbounded.length > 0) {
      val maxDiff = computeMaxDiff(unbounded, bounded)
      assert(maxDiff <= epsilon)
    }
    val big = evaluate(n, d, change, lambda, BigDecimalBoundedProbabilityFinder)
    assert(unbounded.length == big.length)
    if (unbounded.length > 0) {
      val maxDiff1 = computeMaxDiff(unbounded, big)
      assert(maxDiff1 <= epsilon)
      val maxDiff2 = computeMaxDiff(bounded, big)
      assert(maxDiff2 <= epsilon)
    }
  }

  def tortureTest(n: Int, lambda: Int, iterations: Int): Unit = {
    val rng = ThreadLocalRandom.current()
    var count = 0
    while (count < iterations) {
      val (d, change) = if (rng.nextBoolean()) {
        (rng.nextInt(n) + 1, rng.nextInt(n) + 1)
      } else {
        val n9 = n * 9 / 10
        (n9 + 1 + rng.nextInt(n - n9), n9 + 1 + rng.nextInt(n - n9))
      }
      try {
        validate(n, d, change, lambda)
      } catch {
        case e: Throwable =>
          println(s"Failure at n=$n, d=$d, change=$change, lambda=$lambda")
          throw e
      }
      count += 1
    }
  }

  "all methods" should "produce the same results for n=100, d=30, change=3, lambda=1" in validate(100, 30, 3, 1)

  for ((n, d, change, lambda) <- Seq(
    (100, 30, 3, 10),
    (100, 5, 2, 1),
    (100, 5, 2, 10),
    (100, 23, 45, 1),
    (10000, 3858, 2959, 1),
    (1000, 998, 995, 1),
    (1000, 933, 906, 1),
    (1000, 995, 997, 1),
    (10000, 9848, 9808, 1),
    (10000, 9861, 9867, 1),
    (100000, 99085, 81458, 1),
    (100000, 97960, 97574, 100),
  ))
    they should s"produce the same results for n=$n, d=$d, change=$change, lambda=$lambda" in validate(n, d, change, lambda)

  for (n <- Seq(10000, 100000); lambda <- Seq(1, 100, 10000))
    they should s"produce the same results for n=$n, d=10, change=3, lambda=$lambda" in validate(n, 10, 3, lambda)

  for (n <- Seq(100, 1000, 10000, 100000)) {
    they should s"pass the torture test successfully for n = $n" in {
      tortureTest(n, 1, 1000000 / n)
      tortureTest(n, 10, 1000000 / n)
      tortureTest(n, 100, 1000000 / n)
    }
  }
}
