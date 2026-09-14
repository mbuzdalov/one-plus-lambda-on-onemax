package com.github.mbuzdalov.opl.analysis

import java.nio.file.{Files, Path}
import com.github.mbuzdalov.opl.computation.callback.Callback
import com.github.mbuzdalov.util.Loops.{loopFromDownTo, loopFromUntil}

import scala.compiletime.uninitialized
import scala.util.Using

class NonMonotonicitySliceSearch(path: Path, maxDistance: Int) extends Callback[Double] with AutoCloseable:
  private var bestDiff = 0.0
  private var bestDistance = -1
  private var bestParameters: Array[Double] = uninitialized
  private var bestValues: Array[Double] = uninitialized

  override def callBack(distance: Int, parameters: Array[Double], values: Array[Double], bestParameter: Double, bestValue: Double): Unit =
    if distance <= maxDistance then
      val cache = values.clone()
      loopFromUntil(1, cache.length): i =>
        cache(i) = math.min(cache(i), cache(i - 1))
      // cache is a minimum on the suffix
      var minSuffix = Double.PositiveInfinity
      var maxNonConvexity = 0.0
      loopFromDownTo(cache.length - 1, 0): i =>
        val curr = values(i)
        if curr.isFinite then maxNonConvexity = math.max(maxNonConvexity, curr - math.max(cache(i), minSuffix))
        minSuffix = math.min(minSuffix, curr)

      if bestDiff < maxNonConvexity then
        bestDiff = maxNonConvexity
        bestDistance = distance
        bestParameters = parameters.clone()
        bestValues = values.clone()

  override def close(): Unit =
    if bestDiff > 0 then
      Using.resource(Files.newBufferedWriter(path)): writer =>
        writer.write(s"Maximum non-convexity: $bestDiff at distance $bestDistance\n")
        writer.write("Code:\n")
        for i <- bestParameters.indices do writer.write(s"(${bestParameters(i)},${bestValues(i)})")
        writer.newLine()
