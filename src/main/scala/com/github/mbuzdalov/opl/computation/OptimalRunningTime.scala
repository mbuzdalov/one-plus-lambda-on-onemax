package com.github.mbuzdalov.opl.computation

object OptimalRunningTime {
  def newListener: ComputationListener[Int] = new SizedDelegatingListener[Int]((n, l) => new Delegate(n, l))

  class Result private[OptimalRunningTime] (val problemSize: Int,
                                            val populationSize: Int,
                                            expectations: Array[Double],
                                            bestBitFlip: Array[Int],
                                            bitFlipMatrix: Array[Array[Double]]) extends ComputationResult[Int] {
    override def optimalParameter(distance: Int): Int = bestBitFlip(distance - 1)
    override def optimalExpectation(distance: Int): Double = expectations(distance - 1)

    override def optimalExpectationForBitFlips(distance: Int, flips: Int): Double = bitFlipMatrix(distance - 1)(flips - 1)
    override def optimalExpectationForParameter(distance: Int, parameter: Int): Double =
      optimalExpectationForBitFlips(distance, parameter)
  }

  private class Delegate(problemSize: Int, populationSize: Int) extends ComputationListener[Int] {
    private[this] val expectations = new Array[Double](problemSize)
    private[this] val bestBitFlip = new Array[Int](problemSize)
    private[this] val bitFlipMatrix = Array.fill(problemSize, problemSize)(0.0)
    private[this] val tracker = new ConditionalExpectationTracker(expectations)

    override def startComputing(problemSize: Int, populationSize: Int): Unit =
      throw new IllegalStateException("Sizes are already set")

    override def startDistance(distance: Int): Unit = {}

    override def startTransitionProbabilityGroup(distance: Int, change: Int): Unit =
      tracker.reset()

    override def receiveTransitionProbability(change: Int, currDistance: Int, newDistance: Int, probability: Double): Unit =
      tracker.receiveProbability(newDistance, probability)

    override def finishTransitionProbabilityGroup(distance: Int, change: Int): Unit =
      bitFlipMatrix(distance - 1)(change - 1) = (1 + tracker.getConditionalExpectation) / tracker.getUpdateProbability

    override def finishDistance(distance: Int): Unit = {
      var bestFlip = 0
      var bestValue = Double.PositiveInfinity
      var flip = 0
      val bitFlipSlice = bitFlipMatrix(distance - 1)
      while (flip < problemSize) {
        val curr = bitFlipSlice(flip) // intentionally before increment
        flip += 1
        if (bestValue > curr) {
          bestValue = curr
          bestFlip = flip
        }
      }
      expectations(distance - 1) = bestValue
      bestBitFlip(distance - 1) = bestFlip
    }

    override def toResult: ComputationResult[Int] =
      new Result(problemSize, populationSize, expectations, bestBitFlip, bitFlipMatrix)
  }
}
