package com.github.mbuzdalov.opl

import java.io.PrintWriter
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.{Callable, ConcurrentLinkedDeque, CountDownLatch, Executors}

import scala.annotation.tailrec
import scala.util.Using

import org.apache.commons.math3.analysis.MultivariateFunction
import org.apache.commons.math3.optim.nonlinear.scalar.ObjectiveFunction
import org.apache.commons.math3.optim.{InitialGuess, SimpleBounds}
import org.apache.commons.math3.random.{MersenneTwister, RandomGenerator}

import com.github.mbuzdalov.opl.computation.{BareComputationListener, OptimalRunningTime}
import com.github.mbuzdalov.opl.distribution.ParameterizedDistribution
import com.github.mbuzdalov.opl.cma.CMAESOptimizer


object OptimalStaticDistribution {
  private class LatchWrapper(val latch: CountDownLatch, var done: Boolean) {
    def markDone(): Unit = {
      done = true
      latch.countDown()
    }
  }

  private class OnePlusLambdaServer(n: Int, lambda: Int) {
    private val entryLock = new ReentrantLock()
    private val sequence = new ConcurrentLinkedDeque[(BareComputationListener, LatchWrapper)]()

    @tailrec
    final def compute(listener: BareComputationListener): Unit = {
      if (entryLock.tryLock()) {
        val sequenceToEvaluate = IndexedSeq.newBuilder[BareComputationListener]
        val sequenceToNotify = IndexedSeq.newBuilder[LatchWrapper]
        sequenceToEvaluate += listener
        var nextPair = sequence.poll()
        while (nextPair != null) {
          sequenceToEvaluate += nextPair._1
          sequenceToNotify += nextPair._2
          nextPair = sequence.poll()
        }
        val theSequence = sequenceToEvaluate.result()
        OnePlusLambda(n, lambda, theSequence, printTimings = false)
        sequenceToNotify.result().foreach(_.markDone())
        entryLock.unlock()
        val late = sequence.poll()
        if (late != null) {
          late._2.latch.countDown() // as we polled it, it is not done, so give it a chance to work on its own
        }
      } else {
        val myLatch = new LatchWrapper(new CountDownLatch(1), false)
        sequence.add((listener, myLatch))
        myLatch.latch.await()
        if (!myLatch.done) compute(listener)
      }
    }
  }

  private val servers = new scala.collection.mutable.HashMap[(Int, Int), OnePlusLambdaServer]
  private def runOnServer(n: Int, lambda: Int, listener: BareComputationListener): Unit = {
    val myServer = servers.synchronized {
      val key = (n, lambda)
      servers.getOrElseUpdate(key, new OnePlusLambdaServer(n, lambda))
    }
    myServer.compute(listener)
  }

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

  private class FitnessFunction(lambda: Int) extends MultivariateFunction {
    private[this] var nCalls = 0
    private[this] var bestFitness = Double.PositiveInfinity
    private[this] var lastUpdate = 0
    private[this] val fitnessSequence = IndexedSeq.newBuilder[(Int, Double)]

    override def value(distribution: Array[Double]): Double = {
      val n = distribution.length
      val timeForDistributionListener = OptimalRunningTime.newListener(new FixedDistribution(distribution))
      runOnServer(n, lambda, timeForDistributionListener)
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

  private def normalize(a: Array[Double]): Unit = {
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

    val optimizer = new CMAESOptimizer(100 * n * n, true, 10, 10, rng, 10)
    val result = optimizer.optimize(new ObjectiveFunction(objectiveFunction), new InitialGuess(initialGuess),
      new CMAESOptimizer.Sigma(Array.fill(n)(1)),
      new SimpleBounds(Array.fill(n)(0), Array.fill(n)(1)))

    val finalDistribution = result.getPoint
    normalize(finalDistribution)
    RunResult(result.getValue, finalDistribution, objectiveFunction.sequence)
  }

  private val rng = ThreadLocal.withInitial(() => new MersenneTwister())
  private class Task(n: Int, lambda: Int) extends Callable[RunResult] {
    override def call(): RunResult = {
      val start = System.nanoTime()
      val result = findOptimalDistribution(n, lambda, rng.get())
      val time = System.nanoTime() - start
      println(s"  Fitness: ${result.fitness} in ${time * 1e-9} seconds")
      result
    }
  }

  def main(args: Array[String]): Unit = {
    val n = args(0).toInt

    val executor = Executors.newFixedThreadPool(50)

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
