package com.github.mbuzdalov.opl.computation

import com.github.mbuzdalov.opl.TransitionMatrix

trait BareComputationListener {
  def startComputing(problemSize: Int, populationSize: Int): Unit
  def processDistance(distance: Int, matrix: TransitionMatrix): Unit
}
