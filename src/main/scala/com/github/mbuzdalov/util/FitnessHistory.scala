package com.github.mbuzdalov.util

class FitnessHistory(length: Int):
  private val inputStack, outputMin, outputMax = Array.ofDim[Double](length)
  private var nInputs, nOutputs = 0
  private var inputMin = Double.PositiveInfinity
  private var inputMax = Double.NegativeInfinity

  def push(value: Double): Unit =
    if nInputs + nOutputs == capacity then
      if nOutputs == 0 then
        var tailMin = Double.PositiveInfinity
        var tailMax = Double.NegativeInfinity
        while nInputs > 0 do
          nInputs -= 1
          val popped = inputStack(nInputs)
          tailMin = math.min(tailMin, popped)
          tailMax = math.max(tailMax, popped)
          outputMin(nOutputs) = tailMin
          outputMax(nOutputs) = tailMax
          nOutputs += 1
        inputMin = Double.PositiveInfinity
        inputMax = Double.NegativeInfinity
      nOutputs -= 1
    inputStack(nInputs) = value
    nInputs += 1
    inputMin = math.min(inputMin, value)
    inputMax = math.max(inputMax, value)

  def capacity: Int = inputStack.length
  def minimum: Double = if nOutputs == 0 then inputMin else math.min(inputMin, outputMin(nOutputs - 1))
  def maximum: Double = if nOutputs == 0 then inputMax else math.max(inputMax, outputMax(nOutputs - 1))
