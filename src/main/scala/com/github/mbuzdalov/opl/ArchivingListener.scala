package com.github.mbuzdalov.opl

import java.io.DataOutputStream
import java.nio.file.{Files, Path}
import java.util.zip.GZIPOutputStream

import scala.collection.mutable

class ArchivingListener(rootPath: Path, formatter: String, n: Int) extends OnePlusLambdaListener {
  private[this] val streamMap = new mutable.HashMap[Int, DataOutputStream]()

  override def startComputing(lambdas: Seq[Int]): Unit = {
    for (lambda <- lambdas) {
      val file = rootPath.resolve(formatter.format(n, lambda))
      streamMap.put(lambda, new DataOutputStream(new GZIPOutputStream(Files.newOutputStream(file))))
    }
  }

  override def startComputingDistance(lambda: Int, d: Int): Unit = {}

  override def distanceEllComputed(lambda: Int, d: Int, ell: Int,
                                   optimal: Double, drift: Double, driftOptimal: Double): Unit = {
    val data = streamMap(lambda)
    data.writeInt(ell)
    data.writeDouble(optimal)
    data.writeDouble(drift)
    data.writeDouble(driftOptimal)
  }

  override def finishComputingDistance(lambda: Int, d: Int,
                                       optimalValue: Double, optimalEll: Int,
                                       driftOptimalValue: Double, driftOptimalEll: Int, maximalDrift: Double): Unit = {
    val data = streamMap(lambda)
    data.writeDouble(optimalValue)
    data.writeInt(optimalEll)
    data.writeDouble(driftOptimalValue)
    data.writeInt(driftOptimalEll)
    data.writeDouble(maximalDrift)
  }

  override def summary(lambda: Int, expectedOptimal: Double, expectedDriftOptimal: Double): Unit = {
    val data = streamMap(lambda)
    data.writeDouble(expectedOptimal)
    data.writeDouble(expectedDriftOptimal)
  }

  override def finishComputing(): Unit = {
    streamMap.valuesIterator.foreach(_.close())
    streamMap.clear()
  }
}
