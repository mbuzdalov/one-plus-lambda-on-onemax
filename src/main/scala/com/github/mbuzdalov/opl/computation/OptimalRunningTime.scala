package com.github.mbuzdalov.opl.computation

import com.github.mbuzdalov.opl.distribution.ParameterizedDistribution
import com.github.mbuzdalov.opl.{DoubleProbabilityVector, TransitionMatrix}

import scala.reflect.ClassTag

object OptimalRunningTime extends OptimalRunningTime(0)

class OptimalRunningTime(maxOptimumDistance: Int):
  def newListener[@specialized P: ClassTag](distribution: ParameterizedDistribution[P],
                                            optionalCallbackWrapper: Option[callback.Wrapper[P]] = None): ComputationListener[P] =
    SizedDelegatingListener[P]((n, l) => new Delegate(n, l, distribution, optionalCallbackWrapper))

  private class Delegate[@specialized P: ClassTag](problemSize: Int, populationSize: Int,
                                                   distribution: ParameterizedDistribution[P],
                                                   optionalCallbackWrapper: Option[callback.Wrapper[P]])
  extends ComputationListener[P]:
    private val expectations = Array.ofDim[Double](problemSize + 1)
    private val bestParameter = Array.ofDim[P](problemSize)
    private val flipVector, distanceVector = DoubleProbabilityVector(problemSize)

    override def startComputing(problemSize: Int, populationSize: Int): Unit =
      throw IllegalStateException("Sizes are already set")

    private def evaluate(matrix: TransitionMatrix, param: P): Double =
      distribution.initialize(problemSize, param, flipVector)
      distanceVector.setComposition(flipVector, matrix)
      distanceVector.raiseToPowerWithExcessOnSuffix(populationSize)
      val condExp = distanceVector.dotProduct(expectations)
      val totalProb = distanceVector.sum
      (1 + condExp) / totalProb

    override def processDistance(distance: Int, matrix: TransitionMatrix): Unit =
      if distance > maxOptimumDistance then
        val (parameter, value) = distribution.minimize(problemSize, p => evaluate(matrix, p))
        expectations(distance) = value
        bestParameter(distance - 1) = parameter

        optionalCallbackWrapper match
          case Some(callbackWrapper) =>
            for i <- callbackWrapper.parameters.indices do
              callbackWrapper.valuesPlaceholder(i) = evaluate(matrix, callbackWrapper.parameters(i))
            callbackWrapper.run(distance, parameter, value)
          case None =>
      else expectations(distance) = 0.0

    override def toResult: ComputationResult[P] =
      SimpleResult[P](problemSize, populationSize, expectations, bestParameter)
