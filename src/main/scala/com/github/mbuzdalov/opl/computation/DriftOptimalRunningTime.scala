package com.github.mbuzdalov.opl.computation

import com.github.mbuzdalov.opl.distribution.ParameterizedDistribution
import com.github.mbuzdalov.opl.{DoubleProbabilityVector, TransitionMatrix}

import scala.reflect.ClassTag

object DriftOptimalRunningTime {
  def newListener[@specialized P](distribution: ParameterizedDistribution[P],
                                  optionalCallbackWrapper: Option[callback.Wrapper[P]] = None)
                                 (implicit classTag: ClassTag[P]): ComputationListener[P] =
    new SizedDelegatingListener[P]((n, l) => new Delegate(n, l, distribution, optionalCallbackWrapper))

  private class Delegate[@specialized P](problemSize: Int, populationSize: Int,
                                         distribution: ParameterizedDistribution[P],
                                         optionalCallbackWrapper: Option[callback.Wrapper[P]])
                                        (implicit classTag: ClassTag[P]) extends ComputationListener[P] {
    private val expectations = new Array[Double](problemSize + 1)
    private val bestParameter = new Array[P](problemSize)

    private val flipVector, distanceVector = new DoubleProbabilityVector(problemSize)
    private val identity = Array.tabulate(problemSize + 1)(i => i.toDouble)

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
      // we optimize drift instead of time here
      val (parameter, _) = distribution.minimize(problemSize, p => {
        distribution.initialize(problemSize, p, flipVector)
        distanceVector.setComposition(flipVector, matrix)
        distanceVector.raiseToPowerWithExcessOnSuffix(populationSize)
        val totalProb = distanceVector.sum
        -(distance * totalProb - distanceVector.dotProduct(identity))
      })

      expectations(distance) = evaluate(matrix, parameter)
      bestParameter(distance - 1) = parameter

      optionalCallbackWrapper match {
        case Some(callbackWrapper) =>
          for (i <- callbackWrapper.parameters.indices)
            callbackWrapper.valuesPlaceholder(i) = evaluate(matrix, callbackWrapper.parameters(i))
          callbackWrapper.run(distance, parameter, expectations(distance))
        case None =>
      }
    }

    override def toResult: ComputationResult[P] =
      new SimpleResult[P](problemSize, populationSize, expectations, bestParameter)
  }
}
