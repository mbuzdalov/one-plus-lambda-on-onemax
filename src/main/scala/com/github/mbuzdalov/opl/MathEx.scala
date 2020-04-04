package com.github.mbuzdalov.opl

import java.math.{MathContext, RoundingMode}
import java.{util => ju}

import spire.implicits._

object MathEx {
  private[this] var logFactorialCache: Array[Double] = new Array(2)
  private[this] def ensureFactorialExists(n: Int): Unit = {
    if (logFactorialCache.length <= n) {
      assert(n < Int.MaxValue)
      synchronized {
        if (logFactorialCache.length <= n) {
          val newLength = if (n < (1 << 30)) nextPowerOfTwo(n + 1) else Int.MaxValue
          val newArray = new Array[Double](newLength)
          System.arraycopy(logFactorialCache, 0, newArray, 0, logFactorialCache.length)
          var i = logFactorialCache.length
          while (i < newArray.length && i >= 0) {
            newArray(i) = newArray(i - 1) + math.log(i)
            i += 1
          }
          logFactorialCache = newArray
        }
      }
    }
  }

  final val bigLogFactorialContext = new MathContext(40, RoundingMode.HALF_EVEN)
  private[this] val bdLogFactorialCache = new ju.ArrayList[BigDecimal](2)
  bdLogFactorialCache.add(BigDecimal.decimal(0, bigLogFactorialContext))
  bdLogFactorialCache.add(bdLogFactorialCache.get(0))

  private[this] def ensureBDFactorialExists(n: Int): Unit = {
    if (bdLogFactorialCache.size() <= n) {
      synchronized {
        var sz = bdLogFactorialCache.size()
        var last = bdLogFactorialCache.get(sz - 1)
        while (sz <= n) {
          last += BigDecimal.decimal(sz, bigLogFactorialContext).log()
          bdLogFactorialCache.add(last)
          sz += 1
        }
      }
    }
  }

  def logFactorialBig(n: Int): BigDecimal = {
    ensureBDFactorialExists(n)
    bdLogFactorialCache.get(n)
  }

  def logFactorial(n: Int): Double = {
    ensureFactorialExists(n)
    logFactorialCache(n)
  }

  def logChoose(n: Int, k: Int): Double = {
    ensureFactorialExists(n)
    logFactorialCache(n) - logFactorialCache(n - k) - logFactorialCache(k)
  }

  def nextPowerOfTwo(n: Int): Int = {
    require(n <= (1 << 30))
    1 << (32 - Integer.numberOfLeadingZeros(n - 1))
  }
}
