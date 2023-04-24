package com.github.mbuzdalov.oll

import java.util.concurrent.{Callable, ScheduledThreadPoolExecutor}
import java.{util => ju}

import com.github.mbuzdalov.util.NumericMinimization

object BestBinnedSmoothLambda {
  private sealed trait WayToRun {
    def run(n: Int, bins: Seq[Int], lambdaGens: Array[Double], ollComputation: OLLComputation): Double
    def generateInitialIndividual(bins: Seq[Int]): Array[Double]
  }

  private case object NonSmooth extends WayToRun {
    override def run(n: Int, bins: Seq[Int], lambdaGens: Array[Double], ollComputation: OLLComputation): Double =
      RunGivenLambdas.run(
        n = n,
        bins = bins,
        lambdas = lambdaGens,
        populationSizes = lambdaGens.map(v => math.round(v).toInt),
        ollComputation = ollComputation
      )
    override def generateInitialIndividual(bins: Seq[Int]): Array[Double] = Array.fill(bins.size - 1)(1.5)
  }

  private case object Smooth extends WayToRun {
    override def run(n: Int, bins: Seq[Int], lambdaGens: Array[Double], ollComputation: OLLComputation): Double =
      RunGivenLambdas.runSmooth(
        n = n,
        bins = bins,
        lambdas = lambdaGens,
        populationSizes = lambdaGens,
        ollComputation = ollComputation)

    override def generateInitialIndividual(bins: Seq[Int]): Array[Double] = Array.fill(bins.size - 1)(1.5)
  }

  private case object SmoothIndependent extends WayToRun {
    override def run(n: Int, bins: Seq[Int], lambdaGens: Array[Double], ollComputation: OLLComputation): Double =
      RunGivenLambdas.runSmooth(
        n = n,
        bins = bins,
        lambdas = lambdaGens.take(bins.size - 1),
        populationSizes = lambdaGens.drop(bins.size - 1),
        ollComputation = ollComputation)

    override def generateInitialIndividual(bins: Seq[Int]): Array[Double] = Array.fill(2 * (bins.size - 1))(1.5)
  }


  private def run(n: Int, bins: Seq[Int], lambdaGens: Array[Double], tlComputation: ThreadLocal[OLLComputation], way: WayToRun): Double = {
    val ollComp = tlComputation.get()
    ollComp.crossoverComputation.clear()
    way.run(n, bins, lambdaGens, ollComp)
  }

  private def optimize(n: Int, tlComputation: ThreadLocal[OLLComputation], bins: Seq[Int], pool: ScheduledThreadPoolExecutor, way: WayToRun)
                      (a: Array[NumericMinimization.CMAIndividual]): Unit = {
    val list = new ju.ArrayList[Callable[Unit]]
    a.foreach(ind => list.add(() => ind.setRawFitness(run(n, bins, ind.getFixedX, tlComputation, way))))
    pool.invokeAll(list)
  }

  def main(args: Array[String]): Unit = {
    val cmd = new CommandLineArgs(args)
    val n = args(0).toInt

    val tlComputation = ThreadLocal.withInitial[OLLComputation](() => {
      val crossoverComputation = new InMemoryCostPrioritizingCrossoverCache(
        maxCacheByteSize = cmd.getLong("max-cache-byte-size"),
        delegate = CrossoverComputation.findMathCapableImplementation(cmd, "crossover-math"),
        verbose = false)

      new OLLComputation(n,
        neverMutateZeroBits = cmd.getBoolean("never-mutate-zero-bits"),
        includeBestMutantInComparison = cmd.getBoolean("include-best-mutant"),
        ignoreCrossoverParentDuplicates = cmd.getBoolean("ignore-crossover-parent-duplicates"),
        crossoverComputation = crossoverComputation)
    })
    assert(tlComputation.get() != null)

    val wayToRun = cmd.getString("way-to-run", " (expected 'non-smooth', 'smooth', 'independent'") match {
      case "non-smooth" => NonSmooth
      case "smooth" => Smooth
      case "independent" => SmoothIndependent
    }

    val bins = RunGivenLambdas.defaultBins(n)
    val rawLambdaValues = wayToRun.generateInitialIndividual(bins)
    println(s"Bins: ${bins.mkString(", ")}")

    val pool = new ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
    val (result, fitness) = NumericMinimization.optimizeDistributionBySeparableCMAES(
      initialMean = rawLambdaValues,
      lowerBound = _ => 1.0, upperBound = _ => n,
      function = optimize(n, tlComputation, bins, pool, wayToRun),
      initialSigma = cmd.getDouble("initial-sigma"),
      maxIterations = 200, populationSize = 100, nResamplingUntilFeasible = 10,
      logToConsole = true
    )
    pool.shutdown()

    println(s"Final result: ${result.mkString(",")}")
    println(s"Final fitness: $fitness")
  }
}
