package com.github.mbuzdalov.opl.analysis

import java.nio.file.{Files, Path}

import com.github.mbuzdalov.opl.computation.callback.Callback

import scala.collection.mutable.ArrayBuffer
import scala.util.Using

class NonMonotonicityInDistanceSearch(path: Path) extends Callback[Int] with AutoCloseable {
  private var prevBestParameter: Int = _
  private var prevBestValue: Double = Double.PositiveInfinity
  private val strangeness = new ArrayBuffer[Problem]()

  override def callBack(distance: Int, parameters: Array[Int], values: Array[Double], bestParameter: Int, bestValue: Double): Unit = {
    if (prevBestValue.isInfinity) {
      prevBestParameter = bestParameter
      prevBestValue = bestValue
    } else {
      if (prevBestParameter > bestParameter) {
        val prevBestIndex = parameters.indexOf(prevBestParameter)
        strangeness += Problem(distance, prevBestParameter, bestParameter, bestValue, values(prevBestIndex))
      }
    }
  }

  override def close(): Unit = {
    if (strangeness.nonEmpty) {
      Using.resource(Files.newBufferedWriter(path)) { writer =>
        writer.write("distance,prev best,curr best,value at curr best,value at prev best\n")
        for (s <- strangeness) {
          writer.write(s"${s.distance},${s.prevBest},${s.currBest},${s.currAtBest},${s.currAtPrevBest}\n")
        }
      }
    }
  }

  case class Problem(distance: Int, prevBest: Int, currBest: Int, currAtBest: Double, currAtPrevBest: Double)
}
