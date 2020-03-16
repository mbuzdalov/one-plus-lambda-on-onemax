package com.github.mbuzdalov.opl

import java.io.DataOutputStream
import java.nio.file.{Files, Path}
import java.util.zip.GZIPOutputStream

class DeflatingListener(target: Path) extends OnePlusLambdaListener {
  private[this] var data: DataOutputStream = _

  override def startComputing(n: Int, lambda: Int): Unit = {
    data = new DataOutputStream(new GZIPOutputStream(Files.newOutputStream(target)))
  }

  override def startComputingDistance(d: Int): Unit = {}

  override def distanceEllComputed(d: Int, ell: Int,
                                   optimal: Double, drift: Double, driftOptimal: Double): Unit = {
    data.writeInt(ell)
    data.writeDouble(optimal)
    data.writeDouble(drift)
    data.writeDouble(driftOptimal)
  }

  override def finishComputingDistance(d: Int,
                                       optimalValue: Double, optimalEll: Int,
                                       driftOptimalValue: Double, driftOptimalEll: Int, maximalDrift: Double): Unit = {
    data.writeDouble(optimalValue)
    data.writeInt(optimalEll)
    data.writeDouble(driftOptimalValue)
    data.writeInt(driftOptimalEll)
    data.writeDouble(maximalDrift)
  }

  override def finishComputing(expectedOptimal: Double, expectedDriftOptimal: Double): Unit = {
    data.writeDouble(expectedOptimal)
    data.writeDouble(expectedDriftOptimal)
    data.close()
    data = null
  }
}
