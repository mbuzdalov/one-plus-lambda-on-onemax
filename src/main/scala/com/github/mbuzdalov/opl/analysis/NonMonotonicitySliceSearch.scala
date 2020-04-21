package com.github.mbuzdalov.opl.analysis

import java.nio.file.{Files, Path}

import com.github.mbuzdalov.opl.computation.callback.Callback

import scala.util.Using

class NonMonotonicitySliceSearch(path: Path, maxDistance: Int) extends Callback[Double] with AutoCloseable {
  private var bestDiff = 0.0
  private var bestDistance = -1
  private var bestParameters: Array[Double] = _
  private var bestValues: Array[Double] = _

  override def callBack(distance: Int, parameters: Array[Double], values: Array[Double], bestParameter: Double, bestValue: Double): Unit = {
    if (distance <= maxDistance) {
      val cache = values.clone()
      var i = 1
      while (i < cache.length) {
        cache(i) = math.min(cache(i), cache(i - 1))
        i += 1
      }
      // cache is a minimum on the suffix
      var minSuffix = Double.PositiveInfinity
      var maxNonConvexity = 0.0
      i = cache.length - 1
      while (i >= 0) {
        val curr = values(i)
        if (curr.isFinite) {
          maxNonConvexity = math.max(maxNonConvexity, curr - math.max(cache(i), minSuffix))
        }
        minSuffix = math.min(minSuffix, curr)
        i -= 1
      }

      if (bestDiff < maxNonConvexity) {
        bestDiff = maxNonConvexity
        bestDistance = distance
        bestParameters = parameters.clone()
        bestValues = values.clone()
      }
    }
  }

  override def close(): Unit = {
    if (bestDiff > 0) {
      Using.resource(Files.newBufferedWriter(path)) { writer =>
        writer.write(s"Maximum non-convexity: $bestDiff at distance $bestDistance\n")
        writer.write("Code:\n")
        for (i <- bestParameters.indices) {
          writer.write(s"(${bestParameters(i)},${bestValues(i)})")
        }
        writer.newLine()
      }
    }
  }
}
