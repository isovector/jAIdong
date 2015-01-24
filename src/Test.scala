package jaidong

import bwapi.{Unit => BWUnit, _}
import bwta.BaseLocation
import bwta.BWTA
import scala.collection.JavaConversions._


class TestBot extends DefaultBWListener with Prediction {
  val mirror = new Mirror()
  lazy val game: Game = mirror.getGame
  lazy val self: Player = game.self
  var nextExpo: Option[TilePosition] = None

  def run() = {
    mirror.getModule.setEventListener(this)
    mirror.startGame()
  }

  override def onStart(): Unit = {
    game.enableFlag(1)
    game.setLocalSpeed(10)

    println("Analyzing map...");
    BWTA.readMap();
    BWTA.analyze();
    println("Map data ready");
  }

  var isExpanding = false
  override def onFrame(): Unit = {
    updatePrediction()

    game.setTextSize(10);
    game.drawTextScreen(10, 10, "Seconds until 500 minerals: " + (mineralRate.estimateUntil(500) / game.getFPS.toFloat).toInt.toString);

    if (nextExpo.isDefined) {
      val pos = nextExpo.get
      game.drawBoxMap(pos.getX * 32, pos.getY * 32, (pos.getX + 4) * 32, (pos.getY + 3) * 32, bwapi.Color.Orange)
    }

    self.getUnits.foreach { unit =>
      if (unit.getType == UnitType.Terran_Command_Center && self.minerals >= 50) {
        unit.train(UnitType.Terran_SCV);
      }

      if (unit.getType.isWorker && self.minerals >= 500 && !isExpanding) {
        println("I need to expand yo")
        isExpanding = true
        nextExpo = Some(nextExpoLocation(unit))
        unit.build(nextExpo.get, UnitType.Terran_Command_Center)

        val pos = nextExpo.get
        game.setScreenPosition(pos.getX * 32 - 640/2, pos.getY * 32 - 480/2)
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

  override def onUnitComplete(unit: BWUnit): Unit = {
    if (unit.getType == UnitType.Terran_Command_Center) {
      isExpanding = false
      nextExpo = None
    }
  }

  def nextExpoLocation(builder: BWUnit): TilePosition = {
    BWTA.getBaseLocations.toList
      .map(_.getTilePosition)
      .filter(loc => game.canBuildHere(builder, loc, UnitType.Terran_Command_Center))
      .sortBy(_.getDistance(self.getStartLocation))
      .head
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

