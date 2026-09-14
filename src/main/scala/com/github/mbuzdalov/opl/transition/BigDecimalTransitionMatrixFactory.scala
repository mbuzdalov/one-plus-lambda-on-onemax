package com.github.mbuzdalov.opl.transition

import com.github.mbuzdalov.opl.TransitionMatrix
import com.github.mbuzdalov.util.Loops.{loopFromTo, loopFromUntil}
import com.github.mbuzdalov.util.MathEx.logFactorialBig as lF
import spire.implicits.*

object BigDecimalTransitionMatrixFactory extends TransitionMatrixFactory:
  override def create(n: Int, d: Int): TransitionMatrix = Implementation(n, d)

  private class Implementation(n: Int, d: Int) extends TransitionMatrix:
    private val lower, upper = Array.ofDim[Int](n + 1)
    private val improveProbabilities = Array.ofDim[Array[Double]](n + 1)
    private val stayProbabilities = Array.ofDim[Double](n + 1)
    private def init(): Unit =
      loopFromTo(1, n): change =>
        val l = math.max(change / 2 + 1, change - n + d)
        val u = math.min(change, d)
        lower(change) = l
        upper(change) = u

        if l <= u then
          val common = lF(d) + lF(n - d) - lF(n) + lF(change) + lF(n - change)
          val target = Array.ofDim[Double](u - l + 1)
          improveProbabilities(change) = target
          var sum = lF(0)
          loopFromTo(l, u): k =>
            val v = (common - lF(k) - lF(d - k) - lF(change - k) - lF(n - d - change + k)).exp
            target(k - l) = v.toDouble
            sum += v
          stayProbabilities(change) = (1 - sum).toDouble
        else stayProbabilities(change) = 1

    init()

    override def minDistance(change: Int): Int = d - 2 * upper(change) + change
    override def maxDistance(change: Int): Int = d - 2 * lower(change) + change
    override def stepDistance(change: Int): Int = 2

    override def size: Int = n
    override def probability(change: Int, distance: Int): Double =
      if distance > d then 0.0
      else if distance == d then stayProbabilities(change)
      else
        // distance = d - 2 * okay + change => okay = (d - distance + change) / 2
        val okay2 = d - distance + change
        val okay = (d - distance + change) / 2
        if (okay2 & 1) == 1 || lower(change) > okay || okay > upper(change) 
        then 0.0
        else improveProbabilities(change)(okay - lower(change))
  end Implementation
  
