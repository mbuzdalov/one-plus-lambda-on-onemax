package com.github.mbuzdalov.opl

import java.util.Locale

object FractionalExperiment {
  def main(args: Array[String]): Unit = {
    import spire.math.Rational

    val factRat = new Array[Rational](31)
    factRat(0) = Rational(1)
    for (i <- 1 until factRat.length) {
      factRat(i) = factRat(i - 1) * Rational(i)
    }

    def choose(n: Int, k: Int): Rational = factRat(n) / factRat(k) / factRat(n - k)

    def first(d: Int): Unit = {
      println("Power 1")
      val n = 30
      val array = Array.fill(d + 1, 10)(Rational(1))
      for (dd <- 0 until d) {
        print(dd)
        for (rho <- 1 to 10) {
          val div = choose(n, rho)
          if (((rho - d - dd) & 1) == 0 && rho + d >= dd && d >= (rho + d - dd) / 2 && rho - d + dd >= 0 && n - d >= (rho - d + dd) / 2) {
            val prob = choose(d, (rho + d - dd) / 2) * choose(n - d, (rho - d + dd) / 2) / div
            array(dd)(rho - 1) = prob
            array(d)(rho - 1) -= prob
            print(s" & $$\\frac{${prob.numerator}}{${prob.denominator}}$$")
          } else {
            array(dd)(rho - 1) = 0
            print(s" & 0")
          }
        }
        println("\\\\")
      }
      println("\\hline")
      print(d)
      for (rho <- 1 to 10) {
        print(s" & $$\\frac{${array(d)(rho - 1).numerator}}{${array(d)(rho - 1).denominator}}$$")
      }
      println()

      val lambda = 512

      for (rho <- 0 until 10) {
        var prefixSum: Rational = Rational(0)
        for (dd <- d to 0 by -1) {
          val currSum = prefixSum + array(dd)(rho)
          array(dd)(rho) = currSum.pow(lambda) - prefixSum.pow(lambda)
          prefixSum = currSum
        }
      }

      println()
      println("Power lambda")

      val eps = Rational(1, 10).pow(100)
      val half = Rational(1, 2)

      for (dd <- 0 to d) {
        print(dd)
        for (rho <- 0 until 10) {
          val prob = array(dd)(rho)
          if (prob.isZero) {
            print(s" & 0")
          } else if (prob < eps) {
            print(s" & $$< 10^{-100}$$")
          } else if (1 - prob < eps) {
            print(s" & $$> 1 - 10^{-100}$$")
          } else if (prob < half) {
            val formatted = "%.2e".formatLocal(Locale.US, prob.toDouble)
            print(s" & $$${formatted.replace("e-", "\\cdot 10^{-")}}$$")
          } else {
            val formatted = "%.2e".formatLocal(Locale.US, (1 - prob).toDouble)
            print(s" & $$1 - ${formatted.replace("e-", "\\cdot 10^{-")}}$$")
          }
        }
        println("\\\\")
      }

      println()

      print(d)
      for (rho <- 0 until 10) {
        var drift = Rational(0)
        for (dd <- 0 until d) {
          val inc = Rational(d - dd) * array(dd)(rho)
          drift += inc
        }
        val formatted = "%.4f".formatLocal(Locale.US, drift.toDouble)
        print(s" & $formatted")
      }
      println("\\\\")

      println("-----------------")
    }

    first(7)
    println()
    first(8)
  }
}
