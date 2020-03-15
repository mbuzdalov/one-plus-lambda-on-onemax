package com.github.mbuzdalov.opl

import java.io.{BufferedWriter, OutputStreamWriter}
import java.nio.file.{Files, Path}
import java.util.zip.GZIPOutputStream

import scala.collection.mutable

class LegacyArchivingListener(rootPath: Path, formatter: String, n: Int) extends OnePlusLambdaListener {
  private class LambdaData(lambda: Int) {
    val optimalTimeCache, driftMaximizingCache = Array.fill(n)(Double.NaN)
    val optimalByStrength, driftMaximizingByStrength, driftByStrength = Array.fill(n, n)(Double.NaN)

    def close(): Unit = {
      val file = rootPath.resolve(formatter.format(n, lambda))
      val bw = new BufferedWriter(new OutputStreamWriter(new GZIPOutputStream(Files.newOutputStream(file))))
      bw.write(s"$n $lambda\n")
      bw.write(optimalTimeCache.mkString("", " ", "\n"))
      bw.write(driftMaximizingCache.mkString("", " ", "\n"))
      (0 until n).foreach(i => bw.write(optimalByStrength(i).mkString("", " ", "\n")))
      (0 until n).foreach(i => bw.write(driftMaximizingByStrength(i).mkString("", " ", "\n")))
      (0 until n).foreach(i => bw.write(driftByStrength(i).mkString("", " ", "\n")))
      bw.close()
    }
  }

  private[this] val streamMap = new mutable.HashMap[Int, LambdaData]()

  override def startComputing(lambdas: Seq[Int]): Unit = {
    for (lambda <- lambdas) {
      streamMap.put(lambda, new LambdaData(lambda))
    }
  }

  override def startComputingDistance(lambda: Int, d: Int): Unit = {}

  override def distanceEllComputed(lambda: Int, d: Int, ell: Int,
                                   optimal: Double, drift: Double, driftOptimal: Double): Unit = {
    val data = streamMap(lambda)
    data.optimalByStrength(d - 1)(ell - 1) = optimal
    data.driftByStrength(d - 1)(ell - 1) = drift
    data.driftMaximizingByStrength(d - 1)(ell - 1) = driftOptimal
  }

  override def finishComputingDistance(lambda: Int, d: Int,
                                       optimalValue: Double, optimalEll: Int,
                                       driftOptimalValue: Double, driftOptimalEll: Int, maximalDrift: Double): Unit = {
    val data = streamMap(lambda)
    data.optimalTimeCache(d) = optimalValue
    data.driftMaximizingCache(d) = driftOptimalValue
  }

  override def summary(lambda: Int, expectedOptimal: Double, expectedDriftOptimal: Double): Unit = {}

  override def finishComputing(): Unit = {
    streamMap.valuesIterator.foreach(_.close())
    streamMap.clear()
  }
}
