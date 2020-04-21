package com.github.mbuzdalov.opl.computation

import com.github.mbuzdalov.opl.distribution.ParameterizedDistribution
import com.github.mbuzdalov.opl.{DoubleProbabilityVector, TransitionMatrix}

import scala.reflect.ClassTag

object OptimalRunningTime {
  def newListener[@specialized P](distribution: ParameterizedDistribution[P],
                                  optionalCallbackWrapper: Option[callback.Wrapper[P]] = None)
                                 (implicit classTag: ClassTag[P]): ComputationListener[P] =
    new SizedDelegatingListener[P]((n, l) => new Delegate(n, l, distribution, optionalCallbackWrapper))

  private class Delegate[@specialized P](problemSize: Int, populationSize: Int,
                                         distribution: ParameterizedDistribution[P],
                                         optionalCallbackWrapper: Option[callback.Wrapper[P]])
                                        (implicit classTag: ClassTag[P]) extends ComputationListener[P] {
    private[this] val expectations = new Array[Double](problemSize + 1)
    private[this] val bestParameter = new Array[P](problemSize)
    private[this] val flipVector, distanceVector = new DoubleProbabilityVector(problemSize)

    override def startComputing(problemSize: Int, populationSize: Int): Unit =
      throw new IllegalStateException("Sizes are already set")

    private def evaluate(matrix: TransitionMatrix, param: P): Double = {
      distribution.initialize(problemSize, param, flipVector)
      distanceVector.setComposition(flipVector, matrix)
      distanceVector.raiseToPowerWithExcessOnSuffix(populationSize)
      val condExp = distanceVector.dotProduct(expectations)
      val totalProb = distanceVector.sum
      (1 + condExp) / totalProb
    }

    override def processDistance(distance: Int, matrix: TransitionMatrix): Unit = {
      val (parameter, value) = distribution.minimize(problemSize, p => evaluate(matrix, p))
      expectations(distance) = value
      bestParameter(distance - 1) = parameter

      optionalCallbackWrapper match {
        case Some(callbackWrapper) =>
          for (i <- callbackWrapper.parameters.indices)
            callbackWrapper.valuesPlaceholder(i) = evaluate(matrix, callbackWrapper.parameters(i))
          callbackWrapper.run(distance, parameter, value)
        case None =>
      }
    }

    override def toResult: ComputationResult[P] =
      new SimpleResult[P](problemSize, populationSize, expectations, bestParameter)
  }
}
