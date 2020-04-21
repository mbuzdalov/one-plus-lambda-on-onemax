package com.github.mbuzdalov.opl.computation.callback

trait Callback[@specialized P] {
  def callBack(distance: Int, parameters: Array[P], values: Array[Double], bestParameter: P, bestValue: Double): Unit
}
