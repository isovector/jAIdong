package jaidong

import bwapi.{Unit => BWUnit, _}
import bwta.BaseLocation
import bwta.BWTA
import scala.collection.JavaConversions._
import scala.util.{Success, Failure}


import jaidong.util._


class TestBot extends DefaultBWListener with Prediction with BWFutures {
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
    game.setLocalSpeed(0)

    println("Analyzing map...");
    BWTA.readMap();
    BWTA.analyze();
    println("Map data ready");
  }

  override def onFrame(): Unit = {
    promiseMoves()
    updatePrediction()

    game.setTextSize(10);
    game.drawTextScreen(10, 10, "Seconds until 500 minerals: " + (mineralRate.estimateUntil(500) / game.getFPS.toFloat).toInt.toString);

    if (nextExpo.isDefined) {
      val pos = nextExpo.get
      game.drawBoxMap(pos.getX * 32, pos.getY * 32, (pos.getX + 4) * 32, (pos.getY + 3) * 32, bwapi.Color.Orange)
    }

    self.getUnits.foreach { unit =>
      if (unit.getType == UnitType.Terran_Command_Center && self.minerals >= 50 && self.supplyUsed < 16) {
        unit.train(UnitType.Terran_SCV);
      }

      if (unit.getType.isWorker && self.minerals >= 400 && nextExpo.isEmpty) {
        nextExpo = Some(nextExpoLocation(unit))

        val pos = nextExpo.get
        moveTo(unit, new Position(pos.getX * 32, pos.getY * 32)).onComplete {
          case Success(_) =>
            unit.build(pos, UnitType.Terran_Command_Center)

          case Failure(_) =>
            nextExpo = None
        }

        game.setScreenPosition(pos.getX * 32 - 640/2, pos.getY * 32 - 480/2)
      }
    }
  }

  override def onUnitComplete(unit: BWUnit): Unit = {
    if (unit.getType == UnitType.Terran_Command_Center) {
      nextExpo = None
    }

    if (unit.getType.isWorker) {
      val closestMineral =
        game.neutral.getUnits.toList
          .filter(_.getType.isMineralField)
          .sortBy(_.getDistance(unit))
          .head

      unit.gather(closestMineral, false);
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

