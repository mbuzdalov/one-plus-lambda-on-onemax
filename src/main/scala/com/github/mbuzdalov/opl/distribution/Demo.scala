package com.github.mbuzdalov.opl.distribution

import com.github.mbuzdalov.opl.DoubleProbabilityVector

object Demo {
  def make[@specialized P](dist: ParameterizedDistribution[P], n: Int, p: P): DoubleProbabilityVector = {
    val pv = new DoubleProbabilityVector(n)
    dist.initialize(n, p, pv)
    val sum = pv.sum
    pv.setBounds(0, pv.largestDistance)
    if (math.abs(1 - sum) > 1e-10)
      pv.setValue(0, 1 - sum)
    pv
  }

  def main(args: Array[String]): Unit = {
    val generators = Seq[(String, Int => DoubleProbabilityVector)](
      ("SBM", n => make(StandardBitMutation, n, 1.0 / n)),
      ("SBM_{0->1}", n => make(ShiftBitMutation, n, 1.0 / n)),
      ("SBM_{>0}", n => make(ResamplingBitMutation, n, 1.0 / n)),
    )

    println(generators.map(_._1).mkString("n,distance,", ",", ""))

    for (n <- Seq(3, 5, 8, 11, 16, 23, 32, 45, 64, 91, 100)) {
      val vectors = generators.map(_._2.apply(n))

      for (d <- 0 to n)
        println(vectors.map(_.probability(d)).mkString(s"$n,$d,", ",", ""))
    }
  }
}
