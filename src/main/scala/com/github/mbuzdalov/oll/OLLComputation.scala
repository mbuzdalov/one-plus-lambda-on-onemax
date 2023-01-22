package com.github.mbuzdalov.oll

import java.io.PrintWriter

import com.github.mbuzdalov.util.MathEx

/**
 * This class assists in computing the runtime of a given configuration of the (1+(lambda,lambda)) GA.
 * It is stateless and should be driven in an appropriate way by the user.
 *
 * @param n the OneMax problem size.
 * @param neverMutateZeroBits whether one can mutate zero bits in an application of the mutation operator.
 * @param includeBestMutantInComparison whether the best mutant can replace its parent/offspring if it is better.
 * @param ignoreCrossoverParentDuplicates whether one shall count crossover offspring
 *                                        that are identical to either of the parents
 *                                        towards the total count of fitness function evaluations,
 * @param crossoverComputation the particular object to be used for computing the crossover transition probabilities.
 */
class OLLComputation(val n: Int,
                     val neverMutateZeroBits: Boolean,
                     val includeBestMutantInComparison: Boolean,
                     val ignoreCrossoverParentDuplicates: Boolean,
                     val crossoverComputation: CrossoverComputation) {
  /**
   * Finds the runtime assuming the parent's fitness, the value of lambda, and the runtimes for all higher fitness
   * values are given.
   *
   * @param parentFitness the fitness value of the parent.
   * @param lambda the value of lambda, which is used in mutation rates, crossover biases and population sizes.
   * @param runtimes the runtimes for given fitness values.
   * @return the runtime for the given parameters.
   */
  def findRuntime(parentFitness: Int, lambda: Double, populationSize: Int, runtimes: Array[Double]): Double = {
    val mProb = lambda / n
    val xProb = 1 / lambda

    val logMProb = math.log(mProb)
    val log1MProb = math.log1p(-mProb)

    var sumW, sumP = 0.0
    var expectedPopSize = 0.0

    // neverMutateZeroBits #1: we divide the probability of flipping exactly d bits by 1-(1-mProb)^n.
    val mutationScale = if (neverMutateZeroBits)
      1 / (1 - math.exp(n * log1MProb))
    else 1

    // neverMutateZeroBits #2: the expected population size needs to take into account d = 0
    if (!neverMutateZeroBits) {
      val probability = math.exp(n * log1MProb)
      if (ignoreCrossoverParentDuplicates) {
        expectedPopSize += probability * populationSize // all mutations happen, all crossovers ignored
      } else {
        expectedPopSize += probability * 2 * populationSize // all mutations happen, all crossovers happen
      }
    }

    // All that we do we condition on the distance between the parent and offspring.
    var d = 1
    while (d <= n) {

      // The maximum number of good bits to be flipped
      val maxG = math.min(d, n - parentFitness)
      var dProbability, dExpectation = 0.0
      var dCumulativeSum = if (parentFitness < d) 0.0 else math.exp(MathEx.logChoose(parentFitness, d) - MathEx.logChoose(n, d))

      // We need to go from the end: first model the crossover, then mutation atop of it.
      // Everything about crossover, once d, g, popSize and xProb are fixed, is shared across fitness values
      // and can be precomputed (or cached), so we do it.

      var g = 1
      while (g <= maxG) {
        // In mutation, the offspring with most good bits flipped wins.
        // We know we flipped d bits, and there are x bad bits and n-x good bits.
        // In one run, the probability to flip g good bits is choose(n-x, g) * choose(x, d-g) / choose(n, d)
        // We may accumulate the probability in popSize runs just immediately, as well as we may collect the result.
        val pOfThisGInSingleMutation = if (parentFitness < d - g) 0.0 else math.exp(MathEx.logChoose(n - parentFitness, g) + MathEx.logChoose(parentFitness, d - g) - MathEx.logChoose(n, d))
        val newDCumulativeSum = dCumulativeSum + pOfThisGInSingleMutation
        val pOfThisGInAllMutations = if (populationSize > 1) math.pow(newDCumulativeSum, populationSize) - math.pow(dCumulativeSum, populationSize) else pOfThisGInSingleMutation
        dCumulativeSum = newDCumulativeSum

        if (dProbability + pOfThisGInAllMutations > dProbability && (xProb < 1 || g > d - g)) {
          if (xProb == 1) {
            // Everything is flipped, so we basically consider the best mutant.
            // This is a special quick case, as only one possible offspring is generated
            val theFitness = g - (d - g)
            dProbability += pOfThisGInAllMutations
            dExpectation += pOfThisGInAllMutations * runtimes(parentFitness + theFitness)
          } else {
            // Getting the probability of reaching a fitness (probOfReachingF(i) corresponds to fitness x + i)
            val probOfReachingF = crossoverComputation.compute(d, g, populationSize, xProb)
            var xProbOfImprovement, xRemainingTime = 0.0
            // includeBestMutantInComparison: we compute the probabilities of getting all fitness values,
            // but for those smaller than the best mutant, which we know, we use the runtime value
            // corresponding to the best mutant
            val minimumFitnessToUse = if (includeBestMutantInComparison) g - (d - g) else -1
            var i = 0
            while (i < probOfReachingF.length) {
              val nextFitness = parentFitness + math.max(i, minimumFitnessToUse)
              if (nextFitness > parentFitness) {
                xProbOfImprovement += probOfReachingF(i)
                xRemainingTime += probOfReachingF(i) * runtimes(nextFitness)
              }
              i += 1
            }
            dProbability += pOfThisGInAllMutations * xProbOfImprovement
            dExpectation += pOfThisGInAllMutations * xRemainingTime
          }
        }

        g += 1
      }

      assert(math.abs(1 - dCumulativeSum) < 1e-9, "Total probability is not 1")

      // Finally, the probability to flip d bits in mutants is choose(n, d) * mProb^d * (1 - mProb)^(n-d)
      val multipleHere = if (mProb == 1)
        if (n == d) 1.0 else 0.0
      else
        math.exp(MathEx.logChoose(n, d) + d * logMProb + (n - d) * log1MProb) * mutationScale
      assert(dProbability.isFinite, s"dProbability = $dProbability")
      assert(multipleHere.isFinite, s"multipleHere = $multipleHere, d = $d, mProb = $mProb")
      sumP += dProbability * multipleHere
      sumW += dExpectation * multipleHere

      // ignoreCrossoverParentDuplicates: seems that influences only the expected iteration size
      val expectedIterationSize = if (ignoreCrossoverParentDuplicates) {
        populationSize + populationSize * (1 - math.pow(xProb, d) - math.pow(1 - xProb, d))
      } else 2.0 * populationSize

      expectedPopSize += expectedIterationSize * multipleHere

      d += 1
    }

    assert(0 <= sumP && sumP <= 1 + 1e-5, s"Something is terribly wrong: sumP = $sumP, x = $parentFitness, lambda = $lambda")
    if (sumP > 1 + 1e-9) {
      println(s"Warning: sumP = $sumP")
    }
    if (sumP > 1) {
      sumP = 1
    }

    // The final result is straightforward: we wait until success, then go the chosen way,
    // assuming we spend `expectedPopSize` fitness evaluations in each iteration
    (sumW + expectedPopSize) / sumP
  }

  def logConfiguration(out: PrintWriter): Unit = {
    out.print(s"""# n=$n
                 |# --never-mutate-zero-bits=$neverMutateZeroBits
                 |# --include-best-mutant=$includeBestMutantInComparison
                 |# --ignore-crossover-parent-duplicates=$ignoreCrossoverParentDuplicates
                 |""".stripMargin)
  }
}
