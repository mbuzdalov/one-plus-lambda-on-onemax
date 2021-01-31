package com.github.mbuzdalov.opl.util

import scala.annotation.tailrec

import com.github.mbuzdalov.opl.cma.CMAESDistributionOptimizer

object NumericMinimization {
  @tailrec
  def ternarySearch(fun: Double => Double, left: Double, right: Double, iterations: Int, slideRight: Boolean): Double = {
    if (iterations == 0 || left == right) {
      (left + right) / 2
    } else {
      val ll = (left * 2 + right) / 3
      val rr = (left + 2 * right) / 3
      val lv = fun(ll)
      val rv = fun(rr)
      if (lv.isFinite && rv.isFinite) {
        if (lv < rv) {
          ternarySearch(fun, left, rr, iterations - 1, slideRight)
        } else {
          ternarySearch(fun, ll, right, iterations - 1, slideRight)
        }
      } else {
        if (slideRight) {
          ternarySearch(fun, ll, right, iterations - 1, slideRight)
        } else {
          ternarySearch(fun, left, rr, iterations - 1, slideRight)
        }
      }
    }
  }

  def optimizeDistributionBySeparableCMAES(dimension: Int,
                                           function: (Array[Array[Double]], Array[Double]) => Unit,
                                           maxIterations: Int,
                                           populationSize: Int,
                                           nResamplingUntilFeasible: Int): (Array[Double], Double) = {
    val optimizer = new CMAESDistributionOptimizer(maxIterations, nResamplingUntilFeasible, dimension, populationSize, (a, r) => function(a, r))
    (optimizer.getBestIndividual, optimizer.getBestFitness)
  }
}
