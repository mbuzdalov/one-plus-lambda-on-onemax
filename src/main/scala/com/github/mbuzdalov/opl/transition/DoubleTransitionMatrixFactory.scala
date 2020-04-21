package com.github.mbuzdalov.opl.transition

import com.github.mbuzdalov.opl.MathEx.{logFactorial => lF}
import com.github.mbuzdalov.opl.TransitionMatrix

object DoubleTransitionMatrixFactory extends TransitionMatrixFactory {
  override def create(n: Int, d: Int): TransitionMatrix = new Implementation(n, d)

  private class Implementation(n: Int, d: Int) extends TransitionMatrix {
    private[this] val lower, upper = new Array[Int](n + 1)
    private[this] val improveProbabilities = new Array[Array[Double]](n + 1)
    private[this] val stayProbabilities = new Array[Double](n + 1)
    private[this] def init(): Unit = {
      var change = 1
      while (change <= n) {
        val l = math.max(change / 2 + 1, change - n + d)
        val u = math.min(change, d)
        lower(change) = l
        upper(change) = u

        if (l <= u) {
          val common = lF(d) + lF(n - d) - lF(n) + lF(change) + lF(n - change)
          val target = new Array[Double](u - l + 1)
          improveProbabilities(change) = target
          var k = l
          var sum = 0.0
          while (k <= u) {
            val v = math.exp(common - lF(k) - lF(d - k) - lF(change - k) - lF(n - d - change + k))
            target(k - l) = v
            sum += v
            k += 1
          }

          if (target.head == 0 || target.last == 0) {
            var ns = 0
            while (ns < target.length && target(ns) == 0) ns += 1
            var nt = target.length - 1
            while (nt >= ns && target(nt) == 0) nt -= 1
            if (ns <= nt) {
              improveProbabilities(change) = java.util.Arrays.copyOfRange(target, ns, nt + 1)
              lower(change) += ns
              upper(change) -= target.length - 1 - nt
              assert(upper(change) - lower(change) + 1 == improveProbabilities(change).length)
            } else {
              improveProbabilities(change) = null
            }
          }

          stayProbabilities(change) = 1 - sum
        } else {
          stayProbabilities(change) = 1
        }
        change += 1
      }
    }

    init()

    override def minDistance(change: Int): Int = d - 2 * upper(change) + change
    override def maxDistance(change: Int): Int = d - 2 * lower(change) + change
    override def stepDistance(change: Int): Int = 2

    override def size: Int = n
    override def probability(change: Int, distance: Int): Double = {
      if (distance == d)
        stayProbabilities(change)
      else if (distance > d || improveProbabilities(change) == null)
        0.0
      else {
        // distance = d - 2 * okay + change => okay = (d - distance + change) / 2
        val okay2 = d - distance + change
        val okay = (d - distance + change) / 2
        if ((okay2 & 1) == 1 || lower(change) > okay || okay > upper(change))
          0.0
        else
          improveProbabilities(change)(okay - lower(change))
      }
    }
  }
}
