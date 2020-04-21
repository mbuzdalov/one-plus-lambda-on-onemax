package com.github.mbuzdalov.opl.transition

import com.github.mbuzdalov.opl.MathEx.{logFactorialBig => lF}
import com.github.mbuzdalov.opl.TransitionMatrix
import spire.implicits._

object BigDecimalTransitionMatrixFactory extends TransitionMatrixFactory {
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
          var sum = lF(0)
          while (k <= u) {
            val v = (common - lF(k) - lF(d - k) - lF(change - k) - lF(n - d - change + k)).exp()
            target(k - l) = v.toDouble
            sum += v
            k += 1
          }
          stayProbabilities(change) = (1 - sum).toDouble
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
      if (distance > d)
        0.0
      else if (distance == d)
        stayProbabilities(change)
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
