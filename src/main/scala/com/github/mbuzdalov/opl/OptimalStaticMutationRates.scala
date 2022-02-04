package com.github.mbuzdalov.opl

import scala.annotation.tailrec

import com.github.mbuzdalov.opl.computation.OptimalRunningTime
import com.github.mbuzdalov.opl.distribution.ParameterizedDistribution

object OptimalStaticMutationRates {
  private class FixedNonNormalizedDistribution(distribution: Array[Double]) extends ParameterizedDistribution[Unit] {
    override def minimize(n: Int, fun: Unit => Double): (Unit, Double) = {} -> fun({})
    override def initialize(n: Int, param: Unit, target: DoubleProbabilityVector): Unit = {
      assert(n + 1 == distribution.length)
      assert(math.abs(distribution.sum - 1) < 1e-10)
      target.setBounds(0, n)
      var i = 0
      while (i <= n) {
        target.setValue(i, distribution(i))
        i += 1
      }
    }
  }

  private def standard(n: Int, p: Double): Array[Double] = {
    val log1p = math.log(p)
    val logM1p = math.log1p(-p)
    Array.tabulate(n + 1)(i => math.exp(MathEx.logChoose(n, i) + i * log1p + (n - i) * logM1p))
  }

  private def shift(n: Int, p: Double): Array[Double] = {
    val std = standard(n, p)
    std(1) += std(0)
    std(0) = 0
    std
  }

  private def resampling(n: Int, p: Double): Array[Double] = {
    val std = standard(n, p)
    val sum = 1 - std(0)
    std(0) = 0
    for (i <- 0 to n) std(i) /= sum
    std
  }

  private def optimize(n: Int, generator: (Int, Double) => Array[Double]): (Double, Double) = {
    def evaluate(p: Double): Double = {
      val listener = OptimalRunningTime.newListener(new FixedNonNormalizedDistribution(generator(n, p)))
      OnePlusLambda(n, 1, Seq(listener), printTimings = false)
      listener.toResult.expectedRunningTime
    }

    @tailrec
    def go(left: Double, right: Double, iterations: Int): (Double, Double) = {
      if (iterations == 0) {
        val p = (left + right) / 2
        (p, evaluate(p))
      } else {
        val l = (left * 2 + right) / 3
        val r = (left + 2 * right) / 3
        val lv = evaluate(l)
        val rv = evaluate(r)
        print(".")
        if (lv < rv) go(left, r, iterations - 1) else go(l, right, iterations - 1)
      }
    }

    go(0, 1, 100)
  }

  def showFixedTargetTimesForParticularSettings1(): Unit = {
    val n = 1000
    val p = 0.0011106
    val dist = new FixedNonNormalizedDistribution(standard(n, p))
    println("k,p,t")
    val listeners = (500 to 1000).map(k => new OptimalRunningTime(n - k).newListener(dist))
    OnePlusLambda.apply(n, 1, listeners, printTimings = false)
    for ((k, listener) <- (500 to 1000).lazyZip(listeners)) {
      println(s"$k,$p,${listener.toResult.expectedRunningTime}")
    }
  }

  def showFixedTargetTimesForParticularSettings2(): Unit = {
    val n = 1000
    val flips = Seq(1, 3, 5)
    val distributions = flips.map(f => f -> new FixedNonNormalizedDistribution(Array.tabulate(n + 1)(i => if (i == f) 1.0 else 0.0))).toMap
    println(flips.mkString("k,", ",", ""))
    val listeners = for (k <- 500 to 1000; (f, d) <- distributions) yield {
      (k, f, new OptimalRunningTime(n - k).newListener(d))
    }
    OnePlusLambda.apply(n, 1, listeners.map(_._3), printTimings = false)
    for ((k, listenersK) <- listeners.groupBy(_._1).toIndexedSeq.sortBy(_._1)) {
      println(listenersK.sortBy(_._2).map(_._3.toResult.expectedRunningTime).mkString(s"$k,", ",", ""))
    }
  }

  def attemptOptimizingStaticMutationRates(): Unit = {
    val n = 1000
    println(optimize(n, standard))
    println(optimize(n, shift))
    println(optimize(n, resampling))
  }

  def main(args: Array[String]): Unit = {
    showFixedTargetTimesForParticularSettings2()
  }
}
