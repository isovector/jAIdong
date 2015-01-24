package jaidong

import bwapi.{Unit => BWUnit, _}
import bwta.BWTA
import scala.collection.JavaConversions._


class TestBot extends DefaultBWListener with Prediction {
  val mirror = new Mirror()
  lazy val game: Game = mirror.getGame
  lazy val self: Player = game.self

  def run() = {
    mirror.getModule.setEventListener(this)
    mirror.startGame()
  }

  override def onStart(): Unit = {
    game.enableFlag(1)
    game.setLocalSpeed(10)
  }

  override def onFrame(): Unit = {
    updatePrediction()

    game.setTextSize(10);
    game.drawTextScreen(10, 10, "Seconds until 500 minerals: " + (mineralRate.estimateUntil(500) / game.getFPS.toFloat).toString);

    self.getUnits.foreach { unit =>
      if (unit.getType == UnitType.Terran_Command_Center && self.minerals >= 50) {
        unit.train(UnitType.Terran_SCV);
      }

      if (unit.getType.isWorker && unit.isIdle) {
        val closestMineral =
          game.neutral.getUnits.toList
            .filter(_.getType.isMineralField)
            .sortBy(_.getDistance(unit))
            .head

          unit.gather(closestMineral, false);
      }
    }
  }

  override def onEnd(won: Boolean): Unit = {
    System.exit(0)
  }

  override def onPlayerLeft(player: Player): Unit = {
    System.exit(0)
  }
}


object CompilerMain {
  def main(args: Array[String]): Unit = {
    System.setProperty("os.arch", "x86")
    println(System.getProperty("os.arch"))

    new TestBot().run()
  }
}

