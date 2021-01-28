package com.github.mbuzdalov.opl

import com.github.mbuzdalov.opl.computation.OptimalRunningTime
import com.github.mbuzdalov.opl.distribution.ParameterizedDistribution
import org.apache.commons.math3.analysis.MultivariateFunction
import org.apache.commons.math3.optim.nonlinear.scalar.{GoalType, ObjectiveFunction}
import org.apache.commons.math3.optim.{ConvergenceChecker, InitialGuess, MaxEval, PointValuePair, SimpleBounds}
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.CMAESOptimizer
import org.apache.commons.math3.random.{MersenneTwister, RandomGenerator}

import java.io.PrintWriter
import scala.util.Using

object OptimalStaticDistribution {
  class FixedDistribution(distribution: Array[Double]) extends ParameterizedDistribution[Unit] {
    override def minimize(n: Int, fun: Unit => Double): (Unit, Double) = {} -> fun({})
    override def initialize(n: Int, param: Unit, target: DoubleProbabilityVector): Unit = {
      assert(n == distribution.length)
      target.setBounds(1, n)
      val sum = distribution.sum
      var i = 1
      while (i <= n) {
        target.setValue(i, distribution(i - 1) / sum)
        i += 1
      }
    }
  }

  class FitnessFunction(lambda: Int) extends MultivariateFunction {
    private[this] var nCalls = 0
    private[this] var bestFitness = Double.PositiveInfinity
    private[this] var lastUpdate = 0
    private[this] val fitnessSequence = IndexedSeq.newBuilder[(Int, Double)]

    override def value(distribution: Array[Double]): Double = {
      val n = distribution.length
      val timeForDistributionListener = OptimalRunningTime.newListener(new FixedDistribution(distribution))
      OnePlusLambda(n, lambda, Seq(timeForDistributionListener), printTimings = false)
      val result = timeForDistributionListener.toResult.expectedRunningTime
      nCalls += 1
      if (bestFitness > result) {
        if (lastUpdate < nCalls - 1)
          if (bestFitness.isFinite)
            fitnessSequence += (nCalls - 1) -> bestFitness
        lastUpdate = nCalls
        bestFitness = result
        fitnessSequence += nCalls -> bestFitness
      }
      result
    }

    def sequence: IndexedSeq[(Int, Double)] = fitnessSequence.result()
  }

  object NeverConverged extends ConvergenceChecker[PointValuePair] {
    override def converged(iteration: Int, previous: PointValuePair, current: PointValuePair): Boolean = false
  }

  def makeShiftDistribution(n: Int): Array[Double] = {
    import MathEx.{logFactorial => lF}
    val param = 1.0 / n
    val common = lF(n)
    val lP = math.log(param)
    val l1P = math.log1p(-param)
    val result = new Array[Double](n)
    result(0) = math.exp(l1P * n) + math.exp(lP + l1P * (n - 1) + common - lF(1) - lF(n - 1))
    var i = 2
    while (i <= n) {
      result(i - 1) = math.exp(lP * i + l1P * (n - i) + common - lF(i) - lF(n - i))
      i += 1
    }
    result
  }

  def normalize(a: Array[Double]): Unit = {
    val sum = a.sum
    var i = a.length
    while (i > 0) {
      i -= 1
      a(i) /= sum
    }
  }

  case class RunResult(fitness: Double, distribution: Array[Double], history: IndexedSeq[(Int, Double)])

  def findOptimalDistribution(n: Int, lambda: Int, rng: RandomGenerator): RunResult = {
    val objectiveFunction = new FitnessFunction(lambda)
    val initialGuess = Array.fill(n)(rng.nextDouble())
    normalize(initialGuess)

    val optimizer = new CMAESOptimizer(100 * n * n, 0, true, 10,
      10, rng, false, NeverConverged)
    val result = optimizer.optimize(new ObjectiveFunction(objectiveFunction), new InitialGuess(initialGuess),
      GoalType.MINIMIZE, new CMAESOptimizer.PopulationSize(10),
      new CMAESOptimizer.Sigma(Array.fill(n)(1)), new SimpleBounds(Array.fill(n)(0), Array.fill(n)(1)),
      new MaxEval(100 * n * n))

    val finalDistribution = result.getPoint
    normalize(finalDistribution)
    RunResult(result.getValue, finalDistribution, objectiveFunction.sequence)
  }

  def main(args: Array[String]): Unit = {
    val n = args(0).toInt
    val rng = new MersenneTwister()

    Using.resources(new PrintWriter("static-fitness-log.csv"), new PrintWriter("static-distributions.csv")) { (fitnessLog, distLog) =>
      val globalStartTime = System.nanoTime()
      fitnessLog.println("n,lambda,attempt,evaluation,fitness")
      distLog.println("n,lambda,attempt,distance,probability")
      val finalResults = for (lambda <- (1 to 7) ++ Seq(8, 16, 32, 64, 128, 256, 512, 1024)) yield {
        println("Lambda: " + lambda)
        val finalFitnessValues = for (_ <- 0 until 50) yield {
          val start = System.nanoTime()
          val result = findOptimalDistribution(n, lambda, rng)
          val time = System.nanoTime() - start
          println(s"  Fitness: ${result.fitness} in ${time * 1e-9} seconds")
          result
        }
        val min = finalFitnessValues.minBy(_.fitness).fitness
        val max = finalFitnessValues.maxBy(_.fitness).fitness
        val similar = finalFitnessValues.filter(_.fitness < min * (1 + 1e-9))
        similar.zipWithIndex foreach {
          case (RunResult(_, distribution, history), attempt) =>
            for ((i, f) <- history)
              fitnessLog.println(s"$n,$lambda,$attempt,$i,$f")
            for (i <- 1 to distribution.length)
              distLog.println(s"$n,$lambda,$attempt,$i,${distribution(i - 1)}")

        }
        s"lambda = $lambda: min = $min, #similar = ${similar.size}, max = $max"
      }
      finalResults.foreach(println)
      val globalTime = System.nanoTime() - globalStartTime
      println(s"Total time spent: ${globalTime * 1e-9} seconds")
    }
  }
}
