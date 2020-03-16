package com.github.mbuzdalov.opl

import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}

import javax.imageio.ImageIO

import scala.util.Using

class EllPictureBuildingListener(target: Path) extends OnePlusLambdaListener {
  private[this] var currentValues: Array[Double] = _
  private[this] var currentImage: BufferedImage = _

  override def startComputing(n: Int, lambda: Int): Unit = {
    currentValues = new Array[Double](n / 2)
    currentImage = new BufferedImage(n / 2, n / 2, BufferedImage.TYPE_INT_RGB)
  }

  override def startComputingDistance(d: Int): Unit =
    java.util.Arrays.fill(currentValues, Double.NaN)

  override def distanceEllComputed(d: Int, ell: Int,
                                   updateProbability: Double, optimalConditioned: Double,
                                   drift: Double, driftOptimalConditioned: Double): Unit =
    if (ell <= currentValues.length)
      currentValues(ell - 1) = optimalConditioned / updateProbability

  override def finishComputingDistance(d: Int,
                                       optimalValue: Double, optimalEll: Int,
                                       driftOptimalValue: Double, driftOptimalEll: Int, maximalDrift: Double): Unit =
    if (d <= currentValues.length) {
      for (l <- currentValues.indices) {
        val lineValue = currentValues(l)
        val effValue = math.exp(optimalValue - lineValue)
        val pos = (effValue * 255).toInt
        val neg = ((1 - effValue) * 255).toInt
        currentImage.setRGB(d - 1, currentValues.length - l - 1, (pos << 16) ^ neg)
      }
    }

  override def finishComputing(expectedOptimal: Double, expectedDriftOptimal: Double): Unit = {
    Using.resource(Files.newOutputStream(target))(stream => ImageIO.write(currentImage, "png", stream))
    currentImage = null
    currentValues = null
  }
}
