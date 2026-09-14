package com.github.mbuzdalov.opl.computation.callback

import scala.collection.mutable.ArrayBuffer

// @specialized fails here with IllegalAccessError, wtf?
class Wrapper[P](val parameters: Array[P]):
  private val callbacks = ArrayBuffer[Callback[P]]()

  private[computation] final val valuesPlaceholder: Array[Double] = Array.ofDim(parameters.length)

  def add(callback: Callback[P]): Unit = callbacks += callback

  def run(distance: Int, bestParameter: P, bestValue: Double): Unit =
    callbacks.foreach(_.callBack(distance, parameters, valuesPlaceholder, bestParameter, bestValue))
