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
  override def startTransitionProbabilityGroup(change: Int): Unit =
    delegate.startTransitionProbabilityGroup(change)
  override def receiveTransitionProbability(newDistance: Int, probability: Double): Unit =
    delegate.receiveTransitionProbability(newDistance, probability)
  override def finishTransitionProbabilityGroup(): Unit =
    delegate.finishTransitionProbabilityGroup()
  override def finishDistance(): Unit =
    delegate.finishDistance()
  override def toResult: ComputationResult[P] =
    delegate.toResult
}
