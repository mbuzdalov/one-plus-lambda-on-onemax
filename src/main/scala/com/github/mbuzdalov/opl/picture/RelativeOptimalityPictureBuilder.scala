package com.github.mbuzdalov.opl.picture

import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import javax.imageio.ImageIO
import scala.util.Using
import com.github.mbuzdalov.opl.computation.callback.Wrapper
import com.github.mbuzdalov.util.Loops.loopFromUntil
import com.github.mbuzdalov.util.Viridis

class RelativeOptimalityPictureBuilder[@specialized P](target: Path, xMin: Int, xMax: Int, wrapper: Wrapper[P])
extends AutoCloseable:
  private val ordinateValues = wrapper.parameters
  private val image = new BufferedImage(xMax - xMin + 1, ordinateValues.length, BufferedImage.TYPE_INT_RGB)

  wrapper.add: (distance: Int, parameters: Array[P], values: Array[Double], _: P, bestValue: Double) =>
    if xMin <= distance && distance <= xMax then
      assert(parameters eq ordinateValues)
      loopFromUntil(0, ordinateValues.length): yIndex =>
        val currValue = values(yIndex)
        val value01 = math.exp(bestValue - currValue)
        if value01.isNaN then throw IllegalArgumentException(s"best value = $bestValue, curr value = $currValue")
        val rgb = if value01 > 1 then 0xff0000 else Viridis(value01)
        image.setRGB(distance - xMin, ordinateValues.length - yIndex - 1, rgb)

  override def close(): Unit =
    Using.resource(Files.newOutputStream(target))(stream => ImageIO.write(image, "png", stream))
