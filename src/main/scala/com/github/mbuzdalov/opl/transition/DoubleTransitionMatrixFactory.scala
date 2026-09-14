package com.github.mbuzdalov.opl.transition

import com.github.mbuzdalov.opl.TransitionMatrix
import com.github.mbuzdalov.util.Loops.loopFromTo
import com.github.mbuzdalov.util.MathEx.logFactorial as lF

object DoubleTransitionMatrixFactory extends TransitionMatrixFactory:
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
          var sum = 0.0
          loopFromTo(l, u): k =>
            val v = math.exp(common - lF(k) - lF(d - k) - lF(change - k) - lF(n - d - change + k))
            target(k - l) = v
            sum += v

          if target.head == 0 || target.last == 0 then
            var ns = 0
            while ns < target.length && target(ns) == 0 do ns += 1
            var nt = target.length - 1
            while nt >= ns && target(nt) == 0 do nt -= 1
            if ns <= nt then
              improveProbabilities(change) = java.util.Arrays.copyOfRange(target, ns, nt + 1)
              lower(change) += ns
              upper(change) -= target.length - 1 - nt
              assert(upper(change) - lower(change) + 1 == improveProbabilities(change).length)
            else improveProbabilities(change) = null

          stayProbabilities(change) = 1 - sum
        else stayProbabilities(change) = 1

    init()

    override def minDistance(change: Int): Int = d - 2 * upper(change) + change
    override def maxDistance(change: Int): Int = d - 2 * lower(change) + change
    override def stepDistance(change: Int): Int = 2

    override def size: Int = n
    override def probability(change: Int, distance: Int): Double =
      if distance == d then stayProbabilities(change)
      else if distance > d || improveProbabilities(change) == null then 0.0
      else
        // distance = d - 2 * okay + change => okay = (d - distance + change) / 2
        val okay2 = d - distance + change
        val okay = (d - distance + change) / 2
        if (okay2 & 1) == 1 || lower(change) > okay || okay > upper(change) 
        then 0.0
        else improveProbabilities(change)(okay - lower(change))
  end Implementation
