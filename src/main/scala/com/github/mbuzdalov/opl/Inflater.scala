package com.github.mbuzdalov.opl

import java.io.DataInputStream
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream

import scala.util.Using

object Inflater {
  def apply(n: Int, lambda: Int, file: Path, listener: OnePlusLambdaListener): Unit = {
    Using.resource(Files.newInputStream(file)) { inStream =>
      Using.resource(new GZIPInputStream(inStream)) { gzipStream =>
        Using.resource(new DataInputStream(gzipStream)) { data =>
          listener.startComputing(Seq(lambda))
          for (d <- 1 to n) {
            listener.startComputingDistance(lambda, d)
            for (_ <- 1 to n) {
              listener.distanceEllComputed(lambda, d, data.readInt(), data.readDouble(), data.readDouble(), data.readDouble())
            }
            listener.finishComputingDistance(lambda, d, data.readDouble(), data.readInt(), data.readDouble(), data.readInt(), data.readDouble())
          }
          listener.summary(lambda, data.readDouble(), data.readDouble())
          listener.finishComputing()
        }
      }
    }
  }
}
