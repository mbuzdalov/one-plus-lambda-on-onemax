package com.github.mbuzdalov.opl.computation

import com.github.mbuzdalov.opl.distribution.ParameterizedDistribution
import com.github.mbuzdalov.opl.{DoubleProbabilityVector, TransitionMatrix}

import scala.reflect.ClassTag

object DriftOptimalRunningTime:
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
    private val identity = Array.tabulate(problemSize + 1)(i => i.toDouble)

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
      // we optimize drift instead of time here
      val (parameter, _) = distribution.minimize(problemSize, p =>
        distribution.initialize(problemSize, p, flipVector)
        distanceVector.setComposition(flipVector, matrix)
        distanceVector.raiseToPowerWithExcessOnSuffix(populationSize)
        val totalProb = distanceVector.sum
        -(distance * totalProb - distanceVector.dotProduct(identity))
      )

      expectations(distance) = evaluate(matrix, parameter)
      bestParameter(distance - 1) = parameter

      optionalCallbackWrapper match
        case Some(callbackWrapper) =>
          for (i <- callbackWrapper.parameters.indices)
            callbackWrapper.valuesPlaceholder(i) = evaluate(matrix, callbackWrapper.parameters(i))
          callbackWrapper.run(distance, parameter, expectations(distance))
        case None =>

    override def toResult: ComputationResult[P] =
      SimpleResult[P](problemSize, populationSize, expectations, bestParameter)
  