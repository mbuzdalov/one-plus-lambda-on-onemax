package com.github.mbuzdalov.opl

import java.io.BufferedWriter
import java.nio.file.{Files, Path}

import scala.util.Using

import com.github.mbuzdalov.opl.MathEx.LogChoose

class RelativeOptimalityTikZPictureBuilder(probabilities: Seq[Double], standardPath: Path, shiftPath: Path) extends OnePlusLambdaListener {
  private[this] var currentNumerators, currentDenominators: Array[Double] = _
  private[this] var currentN: Int = _
  private[this] var probabilityResultsStandard, probabilityResultsShift: Array[Array[Double]] = _
  private[this] var logChoose: LogChoose = _

  override def startComputing(n: Int, lambda: Int): Unit = {
    currentN = n
    logChoose = new LogChoose(n)
    currentNumerators = new Array[Double](n)
    currentDenominators = new Array[Double](n)
    probabilityResultsStandard = Array.fill(n / 2, probabilities.size)(0.0)
    probabilityResultsShift = Array.fill(n / 2, probabilities.size)(0.0)
  }

  override def startComputingDistance(d: Int): Unit = {
    java.util.Arrays.fill(currentNumerators, Double.NaN)
    java.util.Arrays.fill(currentDenominators, Double.NaN)
  }

  override def distanceEllComputed(d: Int, ell: Int,
                                   updateProbability: Double, optimalConditioned: Double,
                                   drift: Double, driftOptimalConditioned: Double): Unit =
    if (ell <= currentNumerators.length) {
      currentNumerators(ell - 1) = optimalConditioned
      currentDenominators(ell - 1) = updateProbability
    }

  override def finishComputingDistance(d: Int,
                                       optimalValue: Double, optimalEll: Int,
                                       driftOptimalValue: Double, driftOptimalEll: Int, maximalDrift: Double): Unit =
    if (d <= currentN / 2) {
      for (i <- probabilities.indices) {
        val p = probabilities(i) / currentN
        val logP = math.log(p)
        val log1P = math.log(1 - p)
        var resultStandardNum, resultStandardDen, resultShiftNum, resultShiftDen = 0.0
        var ell = 0
        while (ell <= currentN) {
          val prob = math.exp(log1P * (currentN - ell) + logP * ell + logChoose(currentN, ell))
          if (ell != 0) {
            resultStandardNum += prob * currentNumerators(ell - 1)
            resultStandardDen += prob * currentDenominators(ell - 1)
            resultShiftNum += prob * currentNumerators(ell - 1)
            resultShiftDen += prob * currentDenominators(ell - 1)
          } else {
            resultStandardNum += prob
            resultShiftNum += prob * currentNumerators(0)
            resultShiftDen += prob * currentDenominators(0)
          }
          ell += 1
        }
        probabilityResultsStandard(d - 1)(i) = math.exp(optimalValue - (resultStandardNum / resultStandardDen))
        probabilityResultsShift(d - 1)(i) = math.exp(optimalValue - (resultShiftNum / resultShiftDen))
      }
    }

  override def finishComputing(expectedOptimal: Double, expectedDriftOptimal: Double): Unit = {
    Using.resource(Files.newBufferedWriter(standardPath))(bw => write(probabilityResultsStandard, bw))
    Using.resource(Files.newBufferedWriter(shiftPath))(bw => write(probabilityResultsShift, bw))

    currentNumerators = null
    currentDenominators = null
    logChoose = null
    for (i <- probabilities.indices) {
      probabilityResultsStandard(i) = null
      probabilityResultsShift(i) = null
    }
  }

  private def write(array: Array[Array[Double]], target: BufferedWriter): Unit = {
    for (j <- array.indices) {
      for (i <- probabilities.indices) {
        target.write(s"${j + 1} ${probabilities(i)} ${array(j)(i)}")
        target.newLine()
      }
      target.newLine()
    }
  }
}
