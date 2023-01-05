package com.github.mbuzdalov.oll

/**
 * This is an interface to computation and storage of crossover transition probability facilities.
 * The default implementation, that does the math but does not cache anything, is given in the companion object.
 * All other descendants of this trait are expected to perform some sort of caching.
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
  def compute(distanceToParent: Int, goodBitsInDifference: Int, populationSize: Int, crossoverBias: Double): Array[Double]

  /**
   * Cleans up all the resources used by the object.
   */
  def clear(): Unit
}

/**
 * The `CrossoverComputation` companion object is the default implementation of `CrossoverComputation`
 * that does the math but does not store it anywhere. Other implementations are welcome to call `compute` of this
 * companion object and then to manage the results. This object's `compute` is thread-safe and always returns a new array.
 */
object CrossoverComputation extends CrossoverComputation {
  override def compute(distanceToParent: Int, goodBitsInDifference: Int, populationSize: Int, crossoverBias: Double): Array[Double] = {
    val probOfReachingF = Array.ofDim[Double](goodBitsInDifference + 1)

    // For the Hamming distance between the parent and the offspring being `distanceToParent` = d,
    // the number of good bits in the offspring being `goodBitsInDifference` = g,
    // and hence d-g bits flipped wrong in the offspring,
    // reaching fitness f from parent fitness x using crossover bias `crossoverBias` = xProb
    // is possible when flipping f-x+i "good" bits and i "bad" bits for all possible i.
    // That event has the probability choose(g, f-x+i) * choose(d-g, i) * xProb^(f-x+2*i) * (1-xProb)^(d-(f-x+2*i)).

    // It is convenient to rewrite two latter multiples into (1-xProb)^d and (xProb / (1-xProb))^(f-x+2*i),
    // so we precompute the ratio.
    val xOver1X = crossoverBias / (1 - crossoverBias)

    // Now we compute the probabilities of obtaining each fitness f (expressed as fitness delta f-x)
    // by ONE crossover application.
    val badBitsInDifference = distanceToParent - goodBitsInDifference
    var goodFlip = 1
    var p0 = crossoverBias * math.pow(1 - crossoverBias, distanceToParent - 1) * goodBitsInDifference
    while (goodFlip <= goodBitsInDifference) {
      val badFlipLimit = math.min(goodFlip - 1, badBitsInDifference)
      var badFlip = 0
      var p = p0
      while (badFlip <= badFlipLimit) {
        probOfReachingF(goodFlip - badFlip) += p
        p *= xOver1X * (badBitsInDifference - badFlip)
        badFlip += 1
        p /= badFlip
      }
      p0 *= xOver1X * (goodBitsInDifference - goodFlip)
      goodFlip += 1
      p0 /= goodFlip
    }

    // The remaining probability is for being no better
    var probOfReachingFSum = 0.0

    {
      var i = 1
      while (i <= goodBitsInDifference) {
        probOfReachingFSum += probOfReachingF(i)
        i += 1
      }
    }
    probOfReachingF(0) = 1.0 - probOfReachingFSum
    if (probOfReachingF(0) < 0) {
      assert(probOfReachingF(0) >= -1e-9)
      probOfReachingF(0) = 0
    }

    // Now we use populationSize to obtain the final result.
    // The basic idea is that we reach fitness f if all crossover offspring have fitness <= f,
    // but not of them have fitness <= f-1, which results in the infamous subtraction of powers.
    if (populationSize > 1) {
      var sum = 0.0
      probOfReachingFSum = 0
      var i = 0
      while (i <= goodBitsInDifference) {
        val newSum = sum + probOfReachingF(i)
        probOfReachingF(i) = math.pow(newSum, populationSize) - math.pow(sum, populationSize)
        probOfReachingFSum += probOfReachingF(i)
        sum = newSum
        i += 1
      }

      assert(math.abs(1 - sum) < 1e-9, "Total probability is not 1")
      assert(math.abs(1 - probOfReachingFSum) < 1e-9, "Population sizing fails")
    }

    probOfReachingF
  }

  /**
   * Cleans up all the resources used by the object. Does nothing in this stateless implementation.
   */
  override def clear(): Unit = {}
}
