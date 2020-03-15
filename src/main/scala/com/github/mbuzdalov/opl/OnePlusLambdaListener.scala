package com.github.mbuzdalov.opl

trait OnePlusLambdaListener {
  def startComputing(lambdas: Seq[Int]): Unit
  def startComputingDistance(lambda: Int, d: Int): Unit
  def distanceEllComputed(lambda: Int, d: Int, ell: Int, optimal: Double, drift: Double, driftOptimal: Double): Unit
  def finishComputingDistance(lambda: Int, d: Int,
                              optimalValue: Double, optimalEll: Int,
                              driftOptimalValue: Double, driftOptimalEll: Int, maximalDrift: Double): Unit
  def summary(lambda: Int, expectedOptimal: Double, expectedDriftOptimal: Double): Unit
  def finishComputing(): Unit

  def ++ (that: OnePlusLambdaListener): OnePlusLambdaListener = new OnePlusLambdaListener.Composer(that :: this :: Nil)
}

object OnePlusLambdaListener {
  abstract class Adapter extends OnePlusLambdaListener {
    override def startComputing(lambdas: Seq[Int]): Unit = {}
    override def startComputingDistance(lambda: Int, d: Int): Unit = {}
    override def distanceEllComputed(lambda: Int, d: Int, ell: Int,
                                     optimal: Double, drift: Double, driftOptimal: Double): Unit = {}

    override def finishComputingDistance(lambda: Int, d: Int,
                                         optimalValue: Double, optimalEll: Int,
                                         driftOptimalValue: Double, driftOptimalEll: Int, maximalDrift: Double): Unit = {}
    override def summary(lambda: Int, expectedOptimal: Double, expectedDriftOptimal: Double): Unit = {}
    override def finishComputing(): Unit = {}
  }

  object Idle extends Adapter

  private final class Composer(private val all: List[OnePlusLambdaListener]) extends OnePlusLambdaListener {
    override def ++ (that: OnePlusLambdaListener): OnePlusLambdaListener = that match {
      case c: Composer => new Composer(c.all ++ all)
      case e => new Composer(e :: all)
    }

    override def startComputing(lambdas: Seq[Int]): Unit = all.foreach(_.startComputing(lambdas))
    override def startComputingDistance(lambda: Int, d: Int): Unit = all.foreach(_.startComputingDistance(lambda, d))

    override def distanceEllComputed(lambda: Int, d: Int, ell: Int,
                                     optimal: Double, drift: Double, driftOptimal: Double): Unit =
      all.foreach(_.distanceEllComputed(lambda, d, ell, optimal, drift, driftOptimal))

    override def finishComputingDistance(lambda: Int, d: Int,
                                         optimalValue: Double, optimalEll: Int,
                                         driftOptimalValue: Double, driftOptimalEll: Int, maximalDrift: Double): Unit =
      all.foreach(_.finishComputingDistance(lambda, d, optimalValue, optimalEll,
                                            driftOptimalValue, driftOptimalEll, maximalDrift))

    override def summary(lambda: Int, expectedOptimal: Double, expectedDriftOptimal: Double): Unit =
      all.foreach(_.summary(lambda, expectedOptimal, expectedDriftOptimal))

    override def finishComputing(): Unit = all.foreach(_.finishComputing())
  }
}
