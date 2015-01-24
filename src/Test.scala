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

    BWTA.readMap();
    BWTA.analyze();
  }

  override def onFrame(): Unit = {
    runPromises()
    updatePrediction()

    game.setTextSize(10);
    game.drawTextScreen(10, 10, "Seconds until 400 minerals: " + (mineralRate.estimateUntil(500) / game.getFPS.toFloat).toInt.toString);

    if (nextExpo.isDefined) {
      val pos = nextExpo.get
      game.drawBoxMap(pos.getX * 32, pos.getY * 32, (pos.getX + 4) * 32, (pos.getY + 3) * 32, bwapi.Color.Orange)
    }

    self.getUnits.foreach { unit =>
      if (unit.getType == UnitType.Terran_Command_Center && self.minerals >= 50 && self.supplyUsed < 14) {
        unit.train(UnitType.Terran_SCV);
      }

      if (unit.getType.isWorker && self.minerals >= 300 && nextExpo.isEmpty) {
        nextExpo = Some(nextExpoLocation(unit))
        val pos = nextExpo.get
        val length = BWTA.getGroundDistance( unit.getTilePosition, pos)

        synchronize(
          (length / unit.getType.topSpeed).toInt,
          mineralRate.estimateUntil(400)
        ).map { _ =>
          moveTo(unit, new Position(pos.getX * 32, pos.getY * 32)).onComplete {
            case Success(_) =>
              game.setScreenPosition(pos.getX * 32 - 540/2, pos.getY * 32 - 480/2)
              haveEnough(400, 0).map { _ =>
                unit.build(pos, UnitType.Terran_Command_Center)
              }
            case Failure(_) =>
              nextExpo = None
          }
        }
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

  def synchronize(event1: Int, event2: Int) = {
    val diff =
      if (event1 == 0 || event2 == 0)
        0
      else
        math.max(event1, event2) - math.min(event1, event2)
    waitFor(diff)
  }
}


object CompilerMain {
  def main(args: Array[String]): Unit = {
    System.setProperty("os.arch", "x86")
    println(System.getProperty("os.arch"))

    new TestBot().run()
  }
}

