package com.github.mbuzdalov.opl.legacy

import java.awt.image._
import java.io._
import java.util._

import javax.imageio._

import scala.Ordering.Double.IeeeOrdering

object Plotter {
  def main(args: Array[String]): Unit = {
    val root = args(0)
    val out = new PrintWriter(s"$root/plots.tex")
    doAll(root, out)
    out.close()
  }

  private def splitLine(line: String, tokens: String): Array[String] = {
    val st = new StringTokenizer(line, tokens)
    Array.fill(st.countTokens)(st.nextToken())
  }

  private def doAll(root: String, out: PrintWriter): Unit = {
    out.print(s"""\\documentclass{article}
                 |\\usepackage[margin=2cm]{geometry}
                 |\\usepackage{graphicx}
                 |\\begin{document}
                 |""".stripMargin)

    val distanceBuilder = IndexedSeq.newBuilder[(Int, Int, String)]

    for (f0 <- new File(root).list() if f0.startsWith("opl") && f0.endsWith(".txt")) {
      val f = s"$root/$f0"
      val input = new BufferedReader(new FileReader(f))
      val Array(n, lambda) = splitLine(input.readLine(), " ").map(_.toInt)
      println(s"Processing n=$n, lambda=$lambda")
      val rawData = for (d <- 1 to n) yield {
        val line = splitLine(input.readLine(), ": ").map(_.toDouble)
        assert(line.head == d)
        val result = line.tail.toIndexedSeq
        assert(result.size == n + 1)
        result
      }
      input.close()

      val n2 = n / 2
      val image = new BufferedImage(n2, n2, BufferedImage.TYPE_INT_RGB)
      for (d <- 1 to n2) {
        val lineMinimum = rawData(d - 1).min
        for (l <- 1 to n2) {
          val lineValue = rawData(d - 1)(l)
          val effValue = math.exp(lineMinimum - lineValue)
          val pos = (effValue * 255).toInt
          val neg = ((1 - effValue) * 255).toInt
          image.setRGB(d - 1, n2 - l, (pos << 16) ^ neg)
        }
      }
      ImageIO.write(image, "png", new File(s"$root/distance-$n-$lambda.png"))
      distanceBuilder += ((n, lambda, s"""\\begin{figure}
                                         |\\includegraphics[width=\\textwidth,height=0.9\\textheight]{distance-$n-$lambda.png}
                                         |\\caption{Efficiency from Hamming Distance, $$n=$n$$, $$\\lambda=$lambda$$}
                                         |\\end{figure}""".stripMargin))
    }

    distanceBuilder.result().sorted.foreach(t => out.println(t._3))
    out.println("""\end{document}""")
  }
}
