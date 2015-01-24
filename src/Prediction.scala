package jaidong

import bwapi.{Unit => BWUnit, _}
import jaidong.utils.RollingAverage
import scala.collection.JavaConversions._


trait Prediction {
  val game: Game
  val self: Player

  val mineralRate = RollingAverage(10, () => self.minerals)
  val gasRate = RollingAverage(10, () => self.gas)

  def updatePrediction() = {
    val frameCount = game.getFrameCount
    mineralRate.update(frameCount)
    gasRate.update(frameCount)
  }
}
