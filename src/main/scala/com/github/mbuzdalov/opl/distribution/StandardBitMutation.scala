package com.github.mbuzdalov.opl.distribution

import com.github.mbuzdalov.opl.DoubleProbabilityVector
import com.github.mbuzdalov.opl.MathEx.{logFactorial => lF}
import com.github.mbuzdalov.opl.util.NumericMinimization

object StandardBitMutation extends ParameterizedDistribution[Double] {
  override def initialize(n: Int, param: Double, target: DoubleProbabilityVector): Unit = {
    if (param == 0) {
      target.setBounds(1, 0)
    } else if (param == 1) {
      target.setBounds(n, n)
      target.setValue(n, 1.0)
    } else {
      val common = lF(n)
      val lP = math.log(param)
      val l1P = math.log1p(-param)
      target.setBounds(1, n)
      var i = 1
      while (i <= n) {
        target.setValue(i, math.exp(lP * i + l1P * (n - i) + common - lF(i) - lF(n - i)))
        i += 1
      }
    }
  }

  override def minimize(n: Int, fun: Double => Double): (Double, Double) = {
    val atOne = fun(1.0)
    val slideRight = atOne.isFinite
    val result = NumericMinimization.ternarySearch(fun, 0.0, 1.0, 80, slideRight)

    import scala.Ordering.Double.IeeeOrdering
    Seq((1.0, atOne), (result, fun(result))).minBy(_._2)
  }
}
