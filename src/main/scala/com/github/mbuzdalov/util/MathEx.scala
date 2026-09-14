package com.github.mbuzdalov.util

import com.github.mbuzdalov.util.Loops.{loopFromTo, loopFromUntil}

import java.math.{MathContext, RoundingMode}
import java.util as ju
import spire.implicits.*

object MathEx:
  private var logCache: Array[Double] = Array.ofDim(2)
  private var logFactorialCache: Array[Double] = Array.ofDim(2)
  private def ensureFactorialExists(n: Int): Unit =
    if logFactorialCache.length <= n then
      assert(n < Int.MaxValue)
      synchronized:
        if logFactorialCache.length <= n then
          val newLength = if n < (1 << 30) then nextPowerOfTwo(n + 1) else Int.MaxValue
          val newLogArray, newFactArray = Array.ofDim[Double](newLength)
          System.arraycopy(logCache, 0, newLogArray, 0, logFactorialCache.length)
          System.arraycopy(logFactorialCache, 0, newFactArray, 0, logFactorialCache.length)
          var i = logFactorialCache.length
          while i < newFactArray.length && i >= 0 do
            newLogArray(i) = math.log(i)
            newFactArray(i) = newFactArray(i - 1) + newLogArray(i)
            i += 1
          logCache = newLogArray
          logFactorialCache = newFactArray

  private final val bigLogFactorialContext = MathContext(40, RoundingMode.HALF_EVEN)
  private val bdLogFactorialCache = ju.ArrayList[BigDecimal](2)
  bdLogFactorialCache.add(BigDecimal.decimal(0, bigLogFactorialContext))
  bdLogFactorialCache.add(bdLogFactorialCache.get(0))

  private def ensureBDFactorialExists(n: Int): Unit =
    if bdLogFactorialCache.size() <= n then
      synchronized:
        var sz = bdLogFactorialCache.size()
        var last = bdLogFactorialCache.get(sz - 1)
        while sz <= n do
          last += BigDecimal.decimal(sz, bigLogFactorialContext).log
          bdLogFactorialCache.add(last)
          sz += 1

  def logFactorialBig(n: Int): BigDecimal =
    ensureBDFactorialExists(n)
    bdLogFactorialCache.get(n)

  def log(n: Int): Double =
    ensureFactorialExists(n)
    logCache(n)

  def logFactorial(n: Int): Double =
    ensureFactorialExists(n)
    logFactorialCache(n)

  def logChoose(n: Int, k: Int): Double =
    ensureFactorialExists(n)
    logFactorialCache(n) - logFactorialCache(n - k) - logFactorialCache(k)

  def nextPowerOfTwo(n: Int): Int =
    require(n <= (1 << 30))
    1 << (32 - Integer.numberOfLeadingZeros(n - 1))

  def expectedRuntimeOnBitStrings(n: Int, runtimeForFitnessOrDistance: Int => Double): Double =
    var theTotalRuntime = 0.0
    loopFromTo(0, n): x =>
      theTotalRuntime += runtimeForFitnessOrDistance(x) * math.exp(MathEx.logChoose(n, x) - math.log(2) * n)
    theTotalRuntime

  def multiply(array: Array[Double], value: Double): Unit =
    loopFromUntil(0, array.length): i =>
      array(i) *= value
