package com.github.mbuzdalov.opl

import java.util.concurrent.ThreadLocalRandom

import scala.Ordering.Double.IeeeOrdering

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.github.mbuzdalov.opl.transition._

class ProbabilityGenerationTests extends AnyFlatSpec with Matchers {
  private[this] val scaledEpsilon = 3e-14 // 2.2e-14 has once failed

  def evaluate(n: Int, d: Int, change: Int, finder: TransitionProbabilityFinder): Array[Double] = {
    val lower = math.max((change + 1) / 2, change - n + d)
    val upper = math.min(change, d)
    if (lower <= upper) {
      val target = new Array[Double](upper - lower + 1)
      finder.find(n, d, change, target)
      target
    } else {
      new Array(0)
    }
  }

  def computeMaxDiff(a: Array[Double], b: Array[Double]): Double = a.indices.view.map(i => math.abs(a(i) - b(i))).max

  def validate(n: Int, d: Int, change: Int): Unit = {
    val big = evaluate(n, d, change, BigDecimalProbabilityFinder)
    val dbl = evaluate(n, d, change, DoubleProbabilityFinder)
    val epsilon = scaledEpsilon * n
    assert(big.length == dbl.length)
    if (big.length > 0) {
      val maxDiff = computeMaxDiff(big, dbl)
      assert(maxDiff <= epsilon)
    }
  }

  def tortureTest(n: Int, iterations: Int): Unit = {
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
        validate(n, d, change)
      } catch {
        case e: Throwable =>
          println(s"Failure at n=$n, d=$d, change=$change")
          throw e
      }
      count += 1
    }
  }

  "all methods" should "produce the same results for n=100, all d and change" in {
    for (d <- 1 to 100; change <- 1 to 100)
      validate(100, d, change)
  }

  for ((n, d, change) <- Seq(
    (10000, 3858, 2959),
    (1000, 998, 995),
    (1000, 933, 906),
    (1000, 995, 997),
    (10000, 9848, 9808),
    (10000, 9861, 9867),
    (100000, 99085, 81458),
    (100000, 97960, 97574),
  ))
    they should s"produce the same results for n=$n, d=$d, change=$change" in validate(n, d, change)

  for (n <- Seq(10000, 100000))
    they should s"produce the same results for n=$n, d=10, change=3" in validate(n, 10, 3)

  for (n <- Seq(1000, 10000, 100000))
    they should s"pass the torture test successfully for n = $n" in {
      tortureTest(n, math.max(10, 100000 / n))
    }
}
