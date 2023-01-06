package com.github.mbuzdalov.util

import scala.annotation.tailrec

object NumericMinimization {
  @tailrec
  def ternarySearch(fun: Double => Double, left: Double, right: Double, iterations: Int, slideRight: Boolean): Double = {
    if (iterations == 0 || left == right) {
      (left + right) / 2
    } else {
      val ll = (left * 2 + right) / 3
      val rr = (left + 2 * right) / 3
      val lv = fun(ll)
      val rv = fun(rr)
      if (lv.isFinite && rv.isFinite) {
        if (lv < rv) {
          ternarySearch(fun, left, rr, iterations - 1, slideRight)
        } else {
          ternarySearch(fun, ll, right, iterations - 1, slideRight)
        }
      } else {
        if (slideRight) {
          ternarySearch(fun, ll, right, iterations - 1, slideRight)
        } else {
          ternarySearch(fun, left, rr, iterations - 1, slideRight)
        }
      }
    }
  }

  class CMAIndividual(dimension: Int) extends Comparable[CMAIndividual] {
    private val x, z, fixedX = new Array[Double](dimension)
    private var rawFitness, fitness, penalty = 0.0

    @tailrec
    final def initialize(rng: FastRandom,
                         lowerBound: Int => Double,
                         upperBound: Int => Double,
                         xMean: Array[Double],
                         D: Array[Double],
                         sigma: Double,
                         remaining: Int): Unit = {
      penalty = 0
      var i = 0
      while (i < z.length) {
        z(i) = rng.nextGaussian()
        x(i) = xMean(i) + D(i) * z(i) * sigma
        fixedX(i) = math.min(upperBound(i), math.max(lowerBound(i), x(i)))
        penalty += math.abs(x(i) - fixedX(i))
        i += 1
      }
      if (penalty != 0 && remaining > 0) initialize(rng, lowerBound, upperBound, xMean, D, sigma, remaining - 1)
    }

    def getDimension: Int = z.length
    def getFixedX: Array[Double] = fixedX
    def setRawFitness(rawFitness: Double): Unit = this.rawFitness = rawFitness
    def setPenaltyWeight(weight: Double): Unit = this.fitness = this.rawFitness + this.penalty * weight
    def getFitness: Double = fitness
    def getRawFitness: Double = rawFitness

    def getX(index: Int): Double = x(index)
    def getZ(index: Int): Double = z(index)

    override def compareTo(o: CMAIndividual): Int = java.lang.Double.compare(fitness, o.fitness)
  }

  def optimizeDistributionBySeparableCMAES(initialMean: Array[Double],
                                           lowerBound: Int => Double,
                                           upperBound: Int => Double,
                                           function: Array[CMAIndividual] => Unit,
                                           maxIterations: Int,
                                           populationSize: Int,
                                           nResamplingUntilFeasible: Int): (Array[Double], Double) = {
    val dimension = initialMean.length

    // Initialize the common step size.
    var sigma = 1.0

    // Initialize constants for internal termination criteria.
    val stopTolUpX = 1e3
    val stopTolX = 1e-11
    val stopTolFun = 1e-12
    val stopTolHistFun = 1e-13

    // Initialize weights.
    val mu = populationSize / 2
    val weights = Array.tabulate(mu)(i => math.log(mu + 0.5) - math.log(i + 1))
    val sumW = sum(weights)
    val sumWQ = sumSquares(weights)
    val muEff = sumW * sumW / sumWQ
    MathEx.multiply(weights, 1 / sumW)

    // Initialize pre-tuned parameters and constants.
    val cc = (4 + muEff / dimension) / (dimension + 4 + 2 * muEff / dimension)
    val cs = (muEff + 2) / (dimension + muEff + 3)
    val qCS = math.sqrt(cs * (2 - cs) * muEff)
    val damps = (1 + 2 * math.max(0, math.sqrt((muEff - 1) / (dimension + 1)) - 1)) * math.max(0.3, 1 - dimension / (1e-6 + maxIterations)) + cs
    val cCov1 = 2 / ((dimension + 1.3) * (dimension + 1.3) + muEff)
    val cCovMu = math.min(1 - cCov1, 2 * (muEff - 2 + 1 / muEff) / ((dimension + 2.0) * (dimension + 2.0) + muEff))
    val cCov1Sep = math.min(1, cCov1 * (dimension + 1.5) / 3)
    val cCovMuSep = math.min(1 - cCov1, cCovMu * (dimension + 1.5) / 3)
    val chiN = math.sqrt(dimension) * (1 - 1 / (4.0 * dimension) + 1 / (21.0 * dimension * dimension))

    // Initialize matrices and step sizes.
    val D = Array.fill(dimension)(1 / sigma)
    val C = Array.fill(dimension)(1 / sigma / sigma)
    val pc, ps = new Array[Double](dimension)

    // Initialize fitness history to track stagnation.
    val historySize = 10 + (30.0 * dimension / populationSize).toInt
    val fitnessHistory = new FitnessHistory(historySize)

    // Fetch the thread-local random number generator.
    val random = FastRandom.threadLocal

    // Create and evaluate the initial guess.
    val xMean = initialMean.clone()
    var bestRunIndividual = xMean.clone
    var bestRunFitness = {
      val wrapper = new CMAIndividual(dimension)
      wrapper.initialize(random, lowerBound, upperBound, xMean, D, 0.0, 0)
      function(Array(wrapper))
      wrapper.setPenaltyWeight(1)
      wrapper.getFitness
    }

    fitnessHistory.push(bestRunFitness)

    // Allocate all the memory for individuals and auxiliary fitness in/out arrays.
    val individuals = Array.fill(populationSize)(new CMAIndividual(dimension))

    // Do the iterations.
    var iterations = 0
    var continueOptimization = true
    while (continueOptimization && iterations < maxIterations) {
      iterations += 1

      // Initialize the current population.
      individuals.foreach(_.initialize(random, lowerBound, upperBound, xMean, D, sigma, nResamplingUntilFeasible))

      // Run the fitness evaluation on all the fixed individuals.
      function(individuals)

      // Assign violation-discounted fitness to the individuals.
      val valueRange = maxRawFitness(individuals) - minRawFitness(individuals)
      individuals.foreach(_.setPenaltyWeight(valueRange))

      // Sort the individuals. Better ones come first.
      scala.util.Sorting.quickSort(individuals)

      // Update the before-the-matrix step size (commonly known as ps).
      for (i <- 0 until dimension) {
        var zMeanI = 0.0
        var j = 0
        while (j < mu) {
          zMeanI += weights(j) * individuals(j).getZ(i)
          j += 1
        }
        ps(i) = ps(i) * (1 - cs) + zMeanI * qCS
      }

      // Compute normalization coefficients for the rest of the updates.
      val normPS = math.sqrt(sumSquares(ps))
      val hSig = normPS / math.sqrt(1 - math.pow(1 - cs, 2 * iterations)) / chiN < 1.4 + 2 / (dimension + 1.0)
      val q2 = if (hSig) math.sqrt(cc * (2 - cc) * muEff) / sigma else 0
      val oldFac = (1 - cCov1Sep - cCovMuSep) + (if (hSig) 0 else cCov1Sep * cc * (2 - cc))

      // Do all the updates in a single run to avoid storage of temporary variables.
      for (i <- 0 until dimension) {
        val xOldI = xMean(i)
        xMean(i) = 0
        var weighedSquare = 0.0
        var j = 0
        while (j < mu) {
          val xi = individuals(j).getX(i)
          val zi = individuals(j).getZ(i)
          val wj = weights(j)
          xMean(i) += wj * xi
          weighedSquare += wj * zi * zi
          j += 1
        }
        pc(i) = pc(i) * (1 - cc) + q2 * (xMean(i) - xOldI)
        C(i) = C(i) * oldFac + pc(i) * pc(i) * cCov1Sep + C(i) * weighedSquare * cCovMuSep
        D(i) = math.sqrt(C(i))
      }

      // Adapt the common step size.
      sigma *= math.exp(math.min(1, (normPS / chiN - 1) * cs / damps))

      // Update the best individual, and also check up the worst fitness across the iteration.
      val bestFitness = individuals(0).getFitness
      val worstFitness = individuals(populationSize - 1).getFitness
      if (bestRunFitness > bestFitness) {
        bestRunFitness = bestFitness
        bestRunIndividual = individuals(0).getFixedX.clone
      }

      // Collect the data needed for termination condition checks.
      val maxD = max(D)
      val minD = min(D)
      val historyBest = fitnessHistory.minimum
      val historyWorst = fitnessHistory.maximum

      // Check termination conditions.
      if (maxD / minD > 1e7
        || sigma * maxD > stopTolUpX
        || iterations > 2 && math.max(historyWorst, worstFitness) - math.min(historyBest, bestFitness) < stopTolFun
        || iterations > fitnessHistory.capacity && historyWorst - historyBest < stopTolHistFun
        || (0 until dimension).forall(i => !(sigma * math.max(math.abs(pc(i)), D(i)) > stopTolX))) {
        continueOptimization = false
      } else {
        // Adjust step size in the case of equal function values, the case of plain population.
        if (bestRunFitness == individuals((0.1 + populationSize / 4.0).toInt).getFitness) {
          sigma *= math.exp(0.2 + cs / damps)
        }
        // Adjust step size in the case of equal fitness values, the case of plain history.
        if (iterations > 2 && math.max(historyWorst, bestFitness) - math.min(historyBest, bestFitness) == 0) {
          sigma *= math.exp(0.2 + cs / damps)
        }
        // Update the fitness history.
        fitnessHistory.push(bestFitness)
      }
    }

    (bestRunIndividual, bestRunFitness)
  }

  // Warning: this is not the same as array.max due to the possible presence of NaNs
  private def max(array: Array[Double]): Double = {
    var result = Double.NegativeInfinity
    var i = 0
    while (i < array.length) {
      val a = array(i)
      if (result < a) {
        result = a
      }
      i += 1
    }
    result
  }

  // Warning: this is not the same as array.max due to the possible presence of NaNs
  private def maxRawFitness(array: Array[CMAIndividual]): Double = {
    var result = Double.NegativeInfinity
    var i = 0
    while (i < array.length) {
      val a = array(i).getRawFitness
      if (result < a) {
        result = a
      }
      i += 1
    }
    result
  }

  // Warning: this is not the same as array.min due to the possible presence of NaNs
  private def min(array: Array[Double]): Double = {
    var result = Double.PositiveInfinity
    var i = 0
    while (i < array.length) {
      val a = array(i)
      if (result > a) {
        result = a
      }
      i += 1
    }
    result
  }

  // Warning: this is not the same as array.max due to the possible presence of NaNs
  private def minRawFitness(array: Array[CMAIndividual]): Double = {
    var result = Double.PositiveInfinity
    var i = 0
    while (i < array.length) {
      val a = array(i).getRawFitness
      if (result > a) {
        result = a
      }
      i += 1
    }
    result
  }

  private def sum(array: Array[Double]): Double = {
    var result = 0.0
    var i = 0
    while (i < array.length) {
      result += array(i)
      i += 1
    }
    result
  }

  private def sumSquares(array: Array[Double]): Double = {
    var result = 0.0
    var i = 0
    while (i < array.length) {
      result += array(i) * array(i)
      i += 1
    }
    result
  }
}
