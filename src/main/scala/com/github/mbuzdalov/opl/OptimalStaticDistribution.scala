package com.github.mbuzdalov.opl

import java.io.PrintWriter
import java.util.concurrent.{Callable, Executors}

import scala.util.Using

import com.github.mbuzdalov.opl.computation.OptimalRunningTime
import com.github.mbuzdalov.opl.distribution.ParameterizedDistribution
import com.github.mbuzdalov.util.{FastRandom, MathEx, NumericMinimization}

object OptimalStaticDistribution {
  private class FixedDistribution(distribution: Array[Double]) extends ParameterizedDistribution[Unit] {
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

  private class FitnessFunction(lambda: Int) {
    private[this] var nCalls = 0
    private[this] var bestFitness = Double.PositiveInfinity
    private[this] var lastUpdate = 0
    private[this] val fitnessSequence = IndexedSeq.newBuilder[(Int, Double)]

    def evaluate(individuals: Array[NumericMinimization.CMAIndividual]): Unit = {
      val n = individuals(0).getDimension
      val listeners = individuals.map(d => OptimalRunningTime.newListener(new FixedDistribution(d.getFixedX)))

      OnePlusLambda(n, lambda, listeners.toIndexedSeq, printTimings = false)

      for (i <- listeners.indices) {
        val result = listeners(i).toResult.expectedRunningTime
        individuals(i).setRawFitness(result)
        nCalls += 1
        if (bestFitness > result) {
          if (lastUpdate < nCalls - 1)
            if (bestFitness.isFinite)
              fitnessSequence += (nCalls - 1) -> bestFitness
          lastUpdate = nCalls
          bestFitness = result
          fitnessSequence += nCalls -> bestFitness
        }
      }
    }

    def sequence: IndexedSeq[(Int, Double)] = fitnessSequence.result()
  }

  private def normalize(a: Array[Double]): Unit = {
    val sum = a.sum
    var i = a.length
    while (i > 0) {
      i -= 1
      a(i) /= sum
    }
  }

  case class RunResult(fitness: Double, distribution: Array[Double], history: IndexedSeq[(Int, Double)])

  def findOptimalDistribution(n: Int, lambda: Int): RunResult = {
    val objectiveFunction = new FitnessFunction(lambda)
    val rng = FastRandom.threadLocal
    val initialGuess = Array.fill(n)(rng.nextDouble())
    MathEx.multiply(initialGuess, 1.0 / initialGuess.sum)
    val (finalDistribution, finalFitness) = NumericMinimization.optimizeDistributionBySeparableCMAES(initialGuess,
      _ => 0.0, _ => 1.0,
      objectiveFunction.evaluate, 100 * n * n, 10, 10)
    normalize(finalDistribution)
    RunResult(finalFitness, finalDistribution, objectiveFunction.sequence)
  }

  private class Task(n: Int, lambda: Int) extends Callable[RunResult] {
    override def call(): RunResult = {
      val start = System.nanoTime()
      val result = findOptimalDistribution(n, lambda)
      val time = System.nanoTime() - start
      println(s"  Fitness: ${result.fitness} in ${time * 1e-9} seconds")
      result
    }
  }

  def main(args: Array[String]): Unit = {
    val n = args(0).toInt

    val executor = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())

    Using.resources(
      new PrintWriter(s"static-$n-fitness-log.csv"),
      new PrintWriter(s"static-$n-distributions.csv"),
      new PrintWriter(s"static-$n-summary.csv")
    ) { (fitnessLog, distLog, summary) =>
      val globalStartTime = System.nanoTime()
      fitnessLog.println("n,lambda,attempt,evaluation,fitness")
      distLog.println("n,lambda,attempt,distance,probability")
      summary.println("n,lambda,expectation")
      val finalResults = for (lambda <- (1 to 7) ++ Seq(8, 16, 32, 64, 128, 256, 512, 1024)) yield {
        println("Lambda: " + lambda)
        val queue = new java.util.ArrayList[Task]
        for (_ <- 0 until 50) queue.add(new Task(n, lambda))
        val futures = executor.invokeAll(queue)
        val finalFitnessValues = IndexedSeq.tabulate(futures.size())(i => futures.get(i).get())
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
        summary.println(s"$n,$lambda,$min")
        s"lambda = $lambda: min = $min, #similar = ${similar.size}, max = $max"
      }
      finalResults.foreach(println)
      val globalTime = System.nanoTime() - globalStartTime
      println(s"Total time spent: ${globalTime * 1e-9} seconds")
    }

    executor.shutdown()
  }
}
