package com.github.mbuzdalov.opl.computation

import com.github.mbuzdalov.opl.TransitionMatrix

import scala.compiletime.uninitialized

// maybe report a Scala bug: if P is @specialized here and in ComputationListener, delegate is null from the caller
class SizedDelegatingListener[P](constructor: (Int, Int) => ComputationListener[P]) extends ComputationListener[P]:
  private var delegate: ComputationListener[P] = uninitialized

  override def startComputing(problemSize: Int, populationSize: Int): Unit =
    delegate = constructor(problemSize, populationSize)
  override def processDistance(distance: Int, matrix: TransitionMatrix): Unit =
    delegate.processDistance(distance, matrix)
  override def toResult: ComputationResult[P] =
    delegate.toResult
