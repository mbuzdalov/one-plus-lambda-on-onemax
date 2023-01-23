package com.github.mbuzdalov.oll

import com.github.mbuzdalov.oll.xover.{BasicSeparateCrossoverComputation, LegacyCollectiveCrossoverComputation, ZeroCheckingSeparateCrossoverComputation}

/**
 * This is an interface to computation and storage of crossover transition probability facilities.
 * There are some implementations, such as `LegacyCollectiveCrossoverComputation`,
 * that just do the math but does not cache anything.
 * Other implementations of this trait may delegate computation and perform some sort of caching.
 */
trait CrossoverComputation {
  /**
   * Computes the transition probabilities to fitness values equal or higher than the parent's
   * after the crossover is invoked for `populationSize` times
   * assuming the following quantities:
   * - `distanceToParent` is the Hamming distance between the parent and the offspring
   * - `goodBitsInDifference` is the number of good bits that the offspring has but the parent does not have
   * - `crossoverBias` is the probability of taking a bit from the offspring (rather than from the parent).
   *
   * The returned value is an array of `goodBitsInDifference + 1` elements,
   * such that the i-th element is the probability of increasing the parent's fitness by i.
   *
   * @param distanceToParent the Hamming distance between the parent and the offspring.
   * @param goodBitsInDifference the number of good bits that the offspring has but the parent does not have.
   * @param populationSize the population size, that is, how many times the crossover is called.
   * @param crossoverBias the probability of taking a bit from the offspring rather than from the parent.
   * @return the array of probabilities, where the i-th element means the probability of increasing the parent's fitness by i.
   */
  def compute(distanceToParent: Int, goodBitsInDifference: Int, populationSize: Int, crossoverBias: AugmentedProbability): Array[Double]

  /**
   * Cleans up all the resources used by the object.
   */
  def clear(): Unit
}

object CrossoverComputation {
  def findMathCapableImplementation(args: CommandLineArgs, paramName: String): CrossoverComputation = {
    args.getString(paramName, "(expected a name for crossover computation)") match {
      case "legacy" => LegacyCollectiveCrossoverComputation
      case "basic" => BasicSeparateCrossoverComputation
      case "0check" => ZeroCheckingSeparateCrossoverComputation
      case other => throw new IllegalArgumentException(s"I don't know the crossover computation implementation named '$other'")
    }
  }
}
