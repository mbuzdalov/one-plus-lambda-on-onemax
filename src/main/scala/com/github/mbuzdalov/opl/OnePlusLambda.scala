package com.github.mbuzdalov.opl

import java.io.{BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter}
import java.nio.file.{Files, Path}
import java.util.StringTokenizer
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import scala.util.Using

class OnePlusLambda(n: Int, lambda: Int, cacheFileName: Option[Path] = None) {
  private[this] val logChoose = new MathEx.LogChoose(n)
  private[this] val optimalTimeCache, driftMaximizingCache = Array.fill(n)(Double.NaN)
  private[this] val optimalByStrength, driftMaximizingByStrength, driftByStrength = Array.fill(n, n)(Double.NaN)

  cacheFileName match {
    case None =>
      computeEverything()
    case Some(file) =>
      if (Files.exists(file)) {
        readCachesFromFile(file)
      } else {
        computeEverything()
        writeCachesToFile(file)
      }
  }

  private def parseArray(source: String, target: Array[Double]): Unit = {
    val tok = new StringTokenizer(source)
    var i = 0
    while (i < target.length) {
      target(i) = tok.nextToken().toDouble
      i += 1
    }
  }

  private def readCachesFromFile(file: Path): Unit = {
    Using.resource(Files.newInputStream(file)) { fileIn =>
      Using.resource(new GZIPInputStream(fileIn)) { gzipIn =>
        Using.resource(new InputStreamReader(gzipIn)) { gzipReader =>
          Using.resource(new BufferedReader(gzipReader)) { br =>
            val Array(fileN, fileLambda) = br.readLine().split(" ").map(_.toInt)
            require(fileN == n)
            require(fileLambda == lambda)
            parseArray(br.readLine(), optimalTimeCache)
            parseArray(br.readLine(), driftMaximizingCache)
            (0 until n).foreach(i => parseArray(br.readLine(), optimalByStrength(i)))
            (0 until n).foreach(i => parseArray(br.readLine(), driftMaximizingByStrength(i)))
            (0 until n).foreach(i => parseArray(br.readLine(), driftByStrength(i)))
          }
        }
      }
    }
  }

  private def writeCachesToFile(path: Path): Unit = {
    Using.resource(Files.newOutputStream(path)) { fileOut =>
      Using.resource(new GZIPOutputStream(fileOut)) { gzipOut =>
        Using.resource(new OutputStreamWriter(gzipOut)) { gzipWriter =>
          Using.resource(new BufferedWriter(gzipWriter)) { bw =>
            bw.write(s"$n $lambda\n")
            bw.write(optimalTimeCache.mkString("", " ", "\n"))
            bw.write(driftMaximizingCache.mkString("", " ", "\n"))
            (0 until n).foreach(i => bw.write(optimalByStrength(i).mkString("", " ", "\n")))
            (0 until n).foreach(i => bw.write(driftMaximizingByStrength(i).mkString("", " ", "\n")))
            (0 until n).foreach(i => bw.write(driftByStrength(i).mkString("", " ", "\n")))
          }
        }
      }
    }
  }

  private def multiplyByPower(power: Int, unit: ProbabilityVector, result: ProbabilityVector): Unit = {
    var p = power
    while (p > 1) {
      if ((p & 1) == 1) result *= unit
      unit *= unit
      p >>>= 1
    }
    if (p == 1) result *= unit
  }

  def optimalTime(d: Int): Double = if (d == 0) 0.0 else optimalTimeCache(d - 1)

  def optimalExpectedTime: Double = {
    val logAll = math.log(2) * n
    (0 to n).map(d => optimalTime(d) * math.exp(logChoose(n, d) - logAll)).sum
  }

  def driftOptimalTime(d: Int): Double = if (d == 0) 0.0 else driftMaximizingCache(d - 1)

  def driftOptimalExpectedTime: Double = {
    val logAll = math.log(2) * n
    (0 to n).map(d => driftOptimalTime(d) * math.exp(logChoose(n, d) - logAll)).sum
  }

  def optimalTime(d: Int, l: Int): Double =
    if (d == 0)
      0.0
    else if (l == 0)
      Double.PositiveInfinity
    else
      optimalByStrength(d - 1)(l - 1)

  def drift(d: Int, l: Int): Double =
    if (d == 0 || l == 0)
      0.0
    else
      driftByStrength(d - 1)(l - 1)

  def driftOptimalTime(d: Int, l: Int): Double =
    if (d == 0)
      0.0
    else if (l == 0)
      Double.PositiveInfinity
    else
      driftMaximizingByStrength(d - 1)(l - 1)

  private def computeEverything(): Unit = {
    for (d <- 1 to n) {
      var bestDriftValue = -1.0
      var bestDriftIndex = -1
      var minOptimal = Double.PositiveInfinity
      var change = 1
      while (change <= n) {
        compute(d, change)
        minOptimal = math.min(minOptimal, optimalByStrength(d - 1)(change - 1))
        val driftValue = driftByStrength(d - 1)(change - 1)
        if (bestDriftValue < driftValue) {
          bestDriftValue = driftValue
          bestDriftIndex = change
        }
        change += 1
      }
      optimalTimeCache(d - 1) = minOptimal
      driftMaximizingCache(d - 1) = driftMaximizingByStrength(d - 1)(bestDriftIndex - 1)
    }
  }

  private def compute(d: Int, change: Int): Unit = {
    val lower = math.max((change + 1) / 2, change - n + d)
    val upper = math.min(change, d)
    val cnc = logChoose(n, change)
    val prob = new ProbabilityVector(upper - lower + 1)
    for (okay <- 0 to upper - lower) prob.set(okay, math.exp(logChoose(d, okay + lower) + logChoose(n - d, change - okay - lower) - cnc))
    prob.setPreDataByArray()
    if (prob.getPreData < 1) {
      multiplyByPower(lambda - 1, new ProbabilityVector(prob), prob)
    }

    var updateSumOptimal, updateSumDriftOptimal, drift = 0.0
    var updateProb = 0.0
    var i = 0
    while (i < prob.size) {
      val okay = lower + i
      val newD = d - 2 * okay + change
      val pi = prob.get(i)
      if (newD < d) {
        updateProb += pi
        if (newD > 0) {
          updateSumOptimal += optimalTimeCache(newD - 1) * pi
          updateSumDriftOptimal += driftMaximizingCache(newD - 1) * pi
        }
        drift += (d - newD) * pi
      }
      i += 1
    }

    optimalByStrength(d - 1)(change - 1) = (1 + updateSumOptimal) / updateProb
    driftMaximizingByStrength(d - 1)(change - 1) = (1 + updateSumDriftOptimal) / updateProb
    driftByStrength(d - 1)(change - 1) = drift
  }
}
