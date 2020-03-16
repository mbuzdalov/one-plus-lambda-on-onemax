package com.github.mbuzdalov.opl

trait OnePlusLambdaListener {
  def startComputing(n: Int, lambda: Int): Unit
  def startComputingDistance(d: Int): Unit
  def distanceEllComputed(d: Int, ell: Int,
                          updateProbability: Double, optimalConditioned: Double,
                          drift: Double, driftOptimalConditioned: Double): Unit
  def finishComputingDistance(d: Int,
                              optimalValue: Double, optimalEll: Int,
                              driftOptimalValue: Double, driftOptimalEll: Int, maximalDrift: Double): Unit
  def finishComputing(expectedOptimal: Double, expectedDriftOptimal: Double): Unit

  def ++ (that: OnePlusLambdaListener): OnePlusLambdaListener = new OnePlusLambdaListener.Composer(that :: this :: Nil)
}

object OnePlusLambdaListener {
  abstract class Adapter extends OnePlusLambdaListener {
    override def startComputing(n: Int, lambda: Int): Unit = {}
    override def startComputingDistance(d: Int): Unit = {}
    override def distanceEllComputed(d: Int, ell: Int,
                                     updateProbability: Double, optimalConditioned: Double,
                                     drift: Double, driftOptimalConditioned: Double): Unit = {}

    override def finishComputingDistance(d: Int,
                                         optimalValue: Double, optimalEll: Int,
                                         driftOptimalValue: Double, driftOptimalEll: Int, maximalDrift: Double): Unit = {}
    override def finishComputing(expectedOptimal: Double, expectedDriftOptimal: Double): Unit = {}
  }

  object Idle extends Adapter

  private final class Composer(private val all: List[OnePlusLambdaListener]) extends OnePlusLambdaListener {
    override def ++ (that: OnePlusLambdaListener): OnePlusLambdaListener = that match {
      case c: Composer => new Composer(c.all ++ all)
      case e => new Composer(e :: all)
    }

    override def startComputing(n: Int, lambda: Int): Unit = all.foreach(_.startComputing(n, lambda))
    override def startComputingDistance(d: Int): Unit = all.foreach(_.startComputingDistance(d))

    override def distanceEllComputed(d: Int, ell: Int,
                                     updateProbability: Double, optimalConditioned: Double,
                                     drift: Double, driftOptimalConditioned: Double): Unit =
      all.foreach(_.distanceEllComputed(d, ell, updateProbability, optimalConditioned, drift, driftOptimalConditioned))

    override def finishComputingDistance(d: Int,
                                         optimalValue: Double, optimalEll: Int,
                                         driftOptimalValue: Double, driftOptimalEll: Int, maximalDrift: Double): Unit =
      all.foreach(_.finishComputingDistance(d, optimalValue, optimalEll,
                                            driftOptimalValue, driftOptimalEll, maximalDrift))

    override def finishComputing(expectedOptimal: Double, expectedDriftOptimal: Double): Unit =
      all.foreach(_.finishComputing(expectedOptimal, expectedDriftOptimal))
  }
}
