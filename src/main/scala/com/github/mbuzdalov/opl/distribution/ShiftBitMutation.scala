package com.github.mbuzdalov.opl.distribution

import com.github.mbuzdalov.opl.DoubleProbabilityVector
import com.github.mbuzdalov.util.Loops.loopFromTo
import com.github.mbuzdalov.util.NumericMinimization
import com.github.mbuzdalov.util.MathEx.logFactorial as lF

object ShiftBitMutation extends ParameterizedDistribution[Double]:
  override def initialize(n: Int, param: Double, target: DoubleProbabilityVector): Unit =
    if param == 0 then
      target.setBounds(1, 1)
      target.setValue(1, 1.0)
    else if param == 1 then
      target.setBounds(n, n)
      target.setValue(n, 1.0)
    else
      val common = lF(n)
      val lP = math.log(param)
      val l1P = math.log1p(-param)
      target.setBounds(1, n)
      target.setValue(1, math.exp(l1P * n) + math.exp(lP + l1P * (n - 1) + common - lF(1) - lF(n - 1)))
      loopFromTo(2, n): i =>
        target.setValue(i, math.exp(lP * i + l1P * (n - i) + common - lF(i) - lF(n - i)))

  override def minimize(n: Int, fun: Double => Double): (Double, Double) =
    val atOne = fun(1.0)
    val atZero = fun(0.0)
    val slideRight = atZero > atOne
    val small = NumericMinimization.ternarySearch(fun, 0.0, 1.0 / n, 80, slideRight)
    val smallV = fun(small)
    val large = NumericMinimization.ternarySearch(fun, 1.0 / n, 1.0, 80, slideRight)
    val largeV = fun(large)
    val choices = Seq((0.0, atZero), (1.0, atOne), (small, smallV), (large, largeV))
    choices.minBy(_._2)(using scala.Ordering.Double.IeeeOrdering)
