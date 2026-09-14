package com.github.mbuzdalov.opl

import com.github.mbuzdalov.opl.computation.OptimalRunningTime
import com.github.mbuzdalov.opl.distribution.ParameterizedDistribution
import com.github.mbuzdalov.util.Loops.loopFromTo
import com.github.mbuzdalov.util.MathEx

object RuntimesOfAlgorithmsWithStaticDistributions:
  private class FixedNonNormalizedDistribution(distribution: Array[Double]) extends ParameterizedDistribution[Unit]:
    override def minimize(n: Int, fun: Unit => Double): (Unit, Double) = () -> fun(())
    override def initialize(n: Int, param: Unit, target: DoubleProbabilityVector): Unit =
      assert(n + 1 == distribution.length)
      assert(math.abs(distribution.sum - 1) < 1e-12)
      target.setBounds(0, n)
      loopFromTo(0, n)(i => target.setValue(i, distribution(i)))

  private val lambdas = (1 to 7) ++ Seq(8, 16, 32, 64, 128, 256, 512, 1024)

  private def standard(n: Int): Array[Double] =
    val log1p = math.log(1.0 / n)
    val logM1p = math.log1p(-1.0 / n)
    Array.tabulate(n + 1)(i => math.exp(MathEx.logChoose(n, i) + i * log1p + (n - i) * logM1p))
  
  private def shift(n: Int): Array[Double] =
    val std = standard(n)
    std(1) += std(0)
    std(0) = 0
    std

  private def resampling(n: Int): Array[Double] = 
    val std = standard(n)
    val sum = 1 - std(0)
    std(0) = 0
    loopFromTo(0, n)(i => std(i) /= sum)
    std

  private def heavy(n: Int, beta: Double): Array[Double] =
    val heavyLine = Array.tabulate(n + 1)(i => if i == 0 then 0 else math.pow(i, -beta))
    val sum = heavyLine.sum
    loopFromTo(0, n)(i => heavyLine(i) /= sum)
    heavyLine

  private def nevergradHeavy(n: Int, beta: Double): Array[Double] =
    val heavyLine = Array.tabulate(n)(i => math.pow(i + 1, -beta))
    val sum0 = heavyLine.sum
    loopFromTo(0, n)(i => heavyLine(i) /= sum0)

    val result = Array.ofDim[Double](n + 1)
    loopFromTo(1, n): p =>
      if p == n then
        result(n) += heavyLine(p - 1)
      else 
        val log1p = math.log(p.toDouble / n)
        val logM1p = math.log1p(-p.toDouble / n)
        val current = Array.tabulate(n + 1)(i => math.exp(MathEx.logChoose(n, i) + i * log1p + (n - i) * logM1p))
        val currSum = current.sum
        if !(math.abs(currSum - 1) < 1e-12) then throw AssertionError("WTF")
        loopFromTo(0, n)(i => result(i) += current(i) * heavyLine(p - 1))
    val sum = result.sum
    assert(math.abs(sum - 1) < 1e-9)
    loopFromTo(0, n)(i => result(i) /= sum)
    result

  def main(args: Array[String]): Unit =
    println(s"n,lambda,algorithm,expectation")
    for n <- Seq(3, 5, 8, 11, 16, 23, 32, 45, 64, 91, 100); lambda <- lambdas do
      val distributions = Seq(
        "RLS" -> Array.tabulate(n + 1)(i => if (i == 1) 1.0 else 0.0),
        "EA standard" -> standard(n),
        "EA resampling" -> resampling(n),
        "EA shift" -> shift(n),
        "k~beta=1.3" -> heavy(n, 1.3),
        "k~beta=1.5" -> heavy(n, 1.5),
        "k~beta=1.7" -> heavy(n, 1.7),
        "nevergrad beta=1.3" -> nevergradHeavy(n, 1.3),
        "nevergrad beta=1.5" -> nevergradHeavy(n, 1.5),
        "nevergrad beta=1.7" -> nevergradHeavy(n, 1.7),
      )

      val listeners = distributions.map(d => OptimalRunningTime.newListener(FixedNonNormalizedDistribution(d._2)))
      OnePlusLambda(n, lambda, listeners, printTimings = false)

      for ((name, _), listener) <- distributions.lazyZip(listeners) do
        println(s"$n,$lambda,$name,${listener.toResult.expectedRunningTime}")
