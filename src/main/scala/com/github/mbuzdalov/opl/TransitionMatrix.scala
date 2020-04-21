package com.github.mbuzdalov.opl

trait TransitionMatrix {
  def size: Int
  def probability(change: Int, distance: Int): Double
  def minDistance(change: Int): Int
  def maxDistance(change: Int): Int
  def stepDistance(change: Int): Int

  def extractToVector(change: Int, vector: DoubleProbabilityVector): Unit = {
    val minD = minDistance(change)
    val maxD = maxDistance(change)
    vector.setBounds(minD, maxD)
    var d = minD
    while (d <= maxD) {
      vector.setValue(d, probability(change, d))
      d += 1
    }
  }
}
