package com.github.mbuzdalov.opl.computation

object DriftOptimalRunningTime {
  def newListener: ComputationListener[Int] = new SizedDelegatingListener[Int]((n, l) => new Delegate(n, l))

  class Result private[DriftOptimalRunningTime] (val problemSize: Int,
                                                 val populationSize: Int,
                                                 expectations: Array[Double],
                                                 drifts: Array[Double],
                                                 bestBitFlip: Array[Int],
                                                 bitFlipMatrix: Array[Array[Double]]) extends ComputationResult[Int] {
    override def optimalParameter(distance: Int): Int = bestBitFlip(distance - 1)
    override def optimalExpectation(distance: Int): Double = expectations(distance - 1)

    override def optimalExpectationForBitFlips(distance: Int, flips: Int): Double = bitFlipMatrix(distance - 1)(flips - 1)
    override def optimalExpectationForParameter(distance: Int, parameter: Int): Double =
      optimalExpectationForBitFlips(distance, parameter)
  }

  private class Delegate(problemSize: Int, populationSize: Int) extends ComputationListener[Int] {
    private[this] val expectations, drifts, currDrifts = new Array[Double](problemSize)
    private[this] val bestBitFlip = new Array[Int](problemSize)
    private[this] val bitFlipMatrix = Array.fill(problemSize, problemSize)(0.0)

    private[this] var currDistance, currChange: Int = _
    private[this] var currDrift: Double = _
    private[this] val filler = new BitFlipMatrixFiller(expectations, bitFlipMatrix)

    override def startComputing(problemSize: Int, populationSize: Int): Unit =
      throw new IllegalStateException("Sizes are already set")

    override def startDistance(distance: Int): Unit = {
      currDistance = distance
      filler.startDistance(distance)
    }

    override def startTransitionProbabilityGroup(change: Int): Unit = {
      filler.startGroup(change)
      currChange = change
      currDrift = 0
    }

    override def receiveTransitionProbability(newDistance: Int, probability: Double): Unit = {
      filler.receiveProbability(newDistance, probability)
      currDrift += probability * (currDistance - newDistance)
    }

    override def finishTransitionProbabilityGroup(): Unit = {
      filler.finishGroup()
      currDrifts(currChange - 1) = currDrift
    }

    override def finishDistance(): Unit = {
      var bestFlip = 0
      var bestDrift = 0.0
      var bestValue = Double.PositiveInfinity
      var flip = 0
      val bitFlipSlice = bitFlipMatrix(currDistance - 1)
      while (flip < problemSize) {
        val currD = currDrifts(flip)
        val currV = bitFlipSlice(flip) // both intentionally before increment
        flip += 1
        if (bestDrift < currD) {
          bestDrift = currD
          bestValue = currV
          bestFlip = flip
        }
      }
      expectations(currDistance - 1) = bestValue
      drifts(currDistance - 1) = bestDrift
      bestBitFlip(currDistance - 1) = bestFlip
    }

    override def toResult: ComputationResult[Int] =
      new Result(problemSize, populationSize, expectations, drifts, bestBitFlip, bitFlipMatrix)
  }
}
