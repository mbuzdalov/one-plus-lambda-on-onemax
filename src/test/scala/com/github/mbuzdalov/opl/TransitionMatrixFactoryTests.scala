package com.github.mbuzdalov.opl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.github.mbuzdalov.opl.transition._

class TransitionMatrixFactoryTests extends AnyFlatSpec with Matchers {
  private[this] val scaledEpsilon = 3e-14 // 2.2e-14 has once failed

  def computeMaxDiff(a: TransitionMatrix, b: TransitionMatrix, d: Int): Double = {
    val sz = a.size
    var result = 0.0
    var change = 1
    while (change <= sz) {
      var dist = 0
      while (dist <= d) {
        result = math.max(result, math.abs(a.probability(change, dist) - b.probability(change, dist)))
        dist += 1
      }
      change += 1
    }
    result
  }

  def validate(n: Int, d: Int): Unit = {
    val big = BigDecimalTransitionMatrixFactory.create(n, d)
    val dbl = DoubleTransitionMatrixFactory.create(n, d)
    val epsilon = scaledEpsilon * n
    assert(big.size == dbl.size)
    val maxDiff = computeMaxDiff(big, dbl, d)
    assert(maxDiff <= epsilon)
  }

  "all methods" should "produce the same results for n=10, d=5" in validate(10, 5)

  for (d <- 1 to 100)
    they should s"produce the same results for n=100, d=$d" in validate(100, d)

  for ((n, d) <- Seq(
    (1000, 3),
    (1000, 20),
    (1000, 500),
    (1000, 933),
    (1000, 995),
    (1000, 998),
    (10000, 3),
    (10000, 250), // (10000, roughly half) is too long
    (10000, 9948),
    (10000, 9961),
  ))
    they should s"produce the same results for n=$n, d=$d" in validate(n, d)
}
