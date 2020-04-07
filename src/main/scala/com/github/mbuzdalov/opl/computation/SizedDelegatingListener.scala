package com.github.mbuzdalov.opl.computation

// maybe report a Scala bug: if P is @specialized here and in ComputationListener, delegate is null from the caller
class SizedDelegatingListener[P](constructor: (Int, Int) => ComputationListener[P])
  extends ComputationListener[P]
{
  private[this] var delegate: ComputationListener[P] = _

  override def startComputing(problemSize: Int, populationSize: Int): Unit =
    delegate = constructor(problemSize, populationSize)
  override def startDistance(distance: Int): Unit =
    delegate.startDistance(distance)
  override def startTransitionProbabilityGroup(distance: Int, change: Int): Unit =
    delegate.startTransitionProbabilityGroup(distance, change)
  override def receiveTransitionProbability(change: Int, currDistance: Int, newDistance: Int, probability: Double): Unit =
    delegate.receiveTransitionProbability(change, currDistance, newDistance, probability)
  override def finishTransitionProbabilityGroup(distance: Int, change: Int): Unit =
    delegate.finishTransitionProbabilityGroup(distance, change)
  override def finishDistance(distance: Int): Unit =
    delegate.finishDistance(distance)
  override def toResult: ComputationResult[P] =
    delegate.toResult
}
