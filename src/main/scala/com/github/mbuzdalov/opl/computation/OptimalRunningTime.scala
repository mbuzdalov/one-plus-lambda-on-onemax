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

    private[this] var currDecreasedDistance: Int = _
    private[this] val filler = new BitFlipMatrixFiller(expectations, bitFlipMatrix)

    override def startComputing(problemSize: Int, populationSize: Int): Unit =
      throw new IllegalStateException("Sizes are already set")

    override def startDistance(distance: Int): Unit = {
      currDecreasedDistance = distance - 1
      filler.startDistance(distance)
    }

    override def startTransitionProbabilityGroup(change: Int): Unit =
      filler.startGroup(change)

    override def receiveTransitionProbability(newDistance: Int, probability: Double): Unit =
      filler.receiveProbability(newDistance, probability)

    override def finishTransitionProbabilityGroup(): Unit =
      filler.finishGroup()

    override def finishDistance(): Unit = {
      var bestFlip = 0
      var bestValue = Double.PositiveInfinity
      var flip = 0
      val bitFlipSlice = bitFlipMatrix(currDecreasedDistance)
      while (flip < problemSize) {
        val curr = bitFlipSlice(flip) // intentionally before increment
        flip += 1
        if (bestValue > curr) {
          bestValue = curr
          bestFlip = flip
        }
      }
      expectations(currDecreasedDistance) = bestValue
      bestBitFlip(currDecreasedDistance) = bestFlip
    }

    override def toResult: ComputationResult[Int] =
      new Result(problemSize, populationSize, expectations, bestBitFlip, bitFlipMatrix)
  }
}
