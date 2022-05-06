package com.github.mbuzdalov.opl

import java.awt.image.BufferedImage
import java.io.{FileOutputStream, PrintWriter}

import javax.imageio.ImageIO

import scala.util.Using

import com.github.mbuzdalov.opl.computation.{BareComputationListener, BareComputationResult, ComputationListener, OptimalRunningTime}
import com.github.mbuzdalov.opl.distribution.{FlipKBits, ParameterizedDistribution}
import com.github.mbuzdalov.opl.util.Viridis

object OptimalStaticMutationRates {
  private class FixedNonNormalizedDistribution(distribution: Array[Double]) extends ParameterizedDistribution[Unit] {
    override def minimize(n: Int, fun: Unit => Double): (Unit, Double) = {} -> fun({})
    override def initialize(n: Int, param: Unit, target: DoubleProbabilityVector): Unit = {
      assert(n + 1 == distribution.length)
      assert(math.abs(distribution.sum - 1) < 1e-10)
      target.setBounds(0, n)
      var i = 0
      while (i <= n) {
        target.setValue(i, distribution(i))
        i += 1
      }
    }
  }

  private def standard(n: Int, p: Double): Array[Double] = {
    val log1p = math.log(p)
    val logM1p = math.log1p(-p)
    val std = Array.tabulate(n + 1)(i => math.exp(MathEx.logChoose(n, i) + i * log1p + (n - i) * logM1p))
    val sum = std.sum
    for (i <- 0 to n) std(i) /= sum
    std
  }

  def showFixedTargetTimesForParticularSettings1(): Unit = {
    val n = 1000
    val p = 0.0011106
    val dist = new FixedNonNormalizedDistribution(standard(n, p))
    println("k,p,t")
    val listeners = (500 to 1000).map(k => new OptimalRunningTime(n - k).newListener(dist))
    OnePlusLambda.apply(n, 1, listeners, printTimings = false)
    for ((k, listener) <- (500 to 1000).lazyZip(listeners)) {
      println(s"$k,$p,${listener.toResult.expectedRunningTime + 1}")
    }
  }

  def rlsWithFixedNumberOfFlips(): Unit = {
    Using.resource(new PrintWriter("rls-fixed-flips.csv")) { out =>
      val n = 1000
      val flips = Seq(1, 3, 5)
      val distributions = flips.map(f => f -> new FixedNonNormalizedDistribution(Array.tabulate(n + 1)(i => if (i == f) 1.0 else 0.0))).toMap
      out.println(flips.mkString("k,", ",", ""))
      val listeners = for (k <- 500 to 1000; (f, d) <- distributions) yield {
        (k, f, new OptimalRunningTime(n - k).newListener(d))
      }
      OnePlusLambda.apply(n, 1, listeners.map(_._3), printTimings = false)
      for ((k, listenersK) <- listeners.groupBy(_._1).toIndexedSeq.sortBy(_._1)) {
        out.println(listenersK.sortBy(_._2).map(_._3.toResult.expectedRunningTime + 1).mkString(s"$k,", ",", ""))
      }
    }
  }

  def optimalDynamicRLS(): Unit = {
    val n = 1000
    Using.resource(new PrintWriter("rls-optimal-dynamic.csv")) { out =>
      val listener = OptimalRunningTime.newListener(FlipKBits)
      OnePlusLambda.apply(n, 1, Seq(listener), printTimings = false)
      val result = listener.toResult
      out.println("k,f,time")
      for (k <- 999 to 500 by -1) {
        out.println(s"$k,${result.optimalParameter(n - k)},${result.optimalExpectation(n - k)}")
      }
    }
  }

  def targetDynamicRLS(): Unit = {
    val n = 1000
    Using.resource(new PrintWriter("rls-target-dynamic.csv")) { out =>
      val targets = Seq(550, 570, 590, 610, 630, 650, 1000)
      val listeners = targets.map(k => new OptimalRunningTime(n - k).newListener(FlipKBits))
      OnePlusLambda.apply(n, 1, listeners, printTimings = false)
      val results = listeners.map(_.toResult)
      out.println(targets.mkString("k,", ",", ""))
      for (k <- 500 to 700) {
        out.print(k)
        for (r <- results) {
          out.print(",")
          val v = r.optimalExpectation(n - k)
          if (v > 0) {
            out.print(r.optimalParameter(n - k))
          }
        }
        out.println()
      }
    }
  }

  def pictureTargetDynamicRLS(): Unit = {
    val n = 1000
    val vMax = 670

    val targets = (500 to vMax) :+ 1000
    val listeners = targets.map(k => new OptimalRunningTime(n - k).newListener(FlipKBits))
    OnePlusLambda.apply(n, 1, listeners, printTimings = false)
    val results = listeners.map(_.toResult)

    val hh = vMax - 500 + 1
    val heatmap = new BufferedImage(vMax - 500, hh, BufferedImage.TYPE_INT_RGB)
    for (x <- 500 until vMax) {
      val maxFlips = results.last.optimalParameter(n - x)
      for (y <- 500 to vMax) {
        val hx = x - 500
        val hy = hh - 1 - (y - 500)
        lazy val value = results(y - 500).optimalParameter(n - x)
        if (x > y) {
          // undefined
          heatmap.setRGB(hx, hy, 0xffffff)
        } else if (value > maxFlips) {
          heatmap.setRGB(hx, hy, 0xff0000)
          println(s"x = $x, optimal flips = $maxFlips, flips for target $y = $value")
        } else {
          val relativeDiff = (maxFlips - value).toDouble / maxFlips
          // 0 means yellow, 1 means blue
          heatmap.setRGB(hx, hy, Viridis(1 - relativeDiff))
        }
      }
    }
    val heatmapOut = new FileOutputStream("opt-dynamic-heatmap.png")
    ImageIO.write(heatmap, "png", heatmapOut)
    heatmapOut.close()
  }

  def eaWithStaticParameters(): Unit = {
    val n = 1000
    class Task(val target: Int) {
      private[this] var left = 0.0
      private[this] var right = 1.0
      private[this] var midLeftListener, midRightListener, midListener: Option[ComputationListener[Unit]] = None

      private def midLP: Double = (left * 2 + right) / 3
      private def midRP: Double = (left + 2 * right) / 3
      private def midP: Double = (left + right) / 2
      private def createListener(p: Double): ComputationListener[Unit] =
        new OptimalRunningTime(n - target).newListener(new FixedNonNormalizedDistribution(standard(n, p)))

      def nextListeners(isFinalRun: Boolean): Seq[BareComputationListener] = {
        if (midLeftListener.isDefined && midRightListener.isDefined) {
          if (midLeftListener.get.toResult.expectedRunningTime < midRightListener.get.toResult.expectedRunningTime) {
            right = midRP
          } else {
            left = midLP
          }
        }
        if (isFinalRun) {
          midListener = Some(createListener(midP))
          Seq(midListener.get)
        } else {
          midLeftListener = Some(createListener(midLP))
          midRightListener = Some(createListener(midRP))
          Seq(midLeftListener.get, midRightListener.get)
        }
      }

      def finalResult: BareComputationResult = midListener.get.toResult
      def finalRate: Double = midP
    }

    Using.resource(new PrintWriter("1p1-optimal-static-rates.csv")) { out =>
      val tasks = (n / 2 to n).map(t => new Task(t))
      val maxIt = 100
      for (it <- 0 to maxIt) {
        OnePlusLambda.apply(n, 1, tasks.flatMap(_.nextListeners(it == maxIt)), printTimings = false)
        println(s"Iteration $it/$maxIt ended")
      }
      val finalResults = tasks.map(t => (t.target, t.finalRate, t.finalResult))
      val gp = finalResults.last._2
      val gvTasks = (n / 2 to n).map(k => new OptimalRunningTime(n - k).newListener(new FixedNonNormalizedDistribution(standard(n, gp))))
      OnePlusLambda.apply(n, 1, gvTasks, printTimings = false)
      println("Final evaluation done")
      out.println("k,gp*n,gv,lp*n,lv,gv/lv")
      for (((k, lp, lr), gr) <- finalResults.lazyZip(gvTasks)) {
        val lv = lr.expectedRunningTime + 1
        val gv = gr.toResult.expectedRunningTime + 1
        out.println(s"$k,${gp * n},$gv,${lp * n},$lv,${gv / lv}")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    args(0) match {
      case "1p1-optimal-static-rates" => eaWithStaticParameters()
      case "rls-fixed-flips" => rlsWithFixedNumberOfFlips()
      case "rls-optimal-dynamic" => optimalDynamicRLS()
      case "rls-target-dynamic" => targetDynamicRLS()
      case "rls-target-heatmap" => pictureTargetDynamicRLS()
    }
  }
}
