package jaidong

import bwapi.UnitType.{None => Unknown_None, _}
import bwapi.{Unit => BWUnit, _}
import bwta.BaseLocation
import bwta.BWTA
import scala.collection.JavaConversions._
import scala.util.{Success, Failure}

import jaidong.Implicits._
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

    if (nextExpo.isDefined) {
      val tpos = nextExpo.get
      val pos = tpos.toPosition
      game.drawBoxMap(pos.getX, pos.getY, pos.getX + 4 * 32, pos.getY + 3 * 32, bwapi.Color.Orange)
    }

    self.getUnits.foreach { unit =>
      if (unit.getType.isWorker && self.minerals >= 200 && nextExpo.isEmpty) {
        nextExpo = Some(nextExpoLocation(unit))
        val tpos = nextExpo.get
        val pos = tpos.toPosition
        val length = BWTA.getGroundDistance(unit.getTilePosition, tpos)

        synchronize(
          (length / unit.getType.topSpeed).toInt,
          mineralRate.estimateUntil(300)
        ).flatMap { _ =>
          moveTo(unit, pos)
        }.flatMap { _ =>
          game.setScreenPosition(pos.getX - 640/2, pos.getY - 330/2)
          haveEnough(300, 0)
        }.map { _ =>
          unit.build(tpos, Zerg_Hatchery)
        }
      }

      if (unit is Zerg_Larva)
        unit.morph(Zerg_Drone)
    }
  }

  override def onUnitComplete(unit: BWUnit): Unit = {
    if (game.getFrameCount < waitLatency) {
      onUnitMorph(unit)
    }
    println("on complete " + unit.getType.toString)
  }

  override def onUnitMorph(unit: BWUnit): Unit = {
    println("on morph " + unit.getType.toString)
    if (unit is Zerg_Hatchery) {
      nextExpo = None
    }

    if (unit.getType.isWorker) {
      val closestMineral =
        game.neutral.getUnits.toList
          .filter(_.getType.isMineralField)
          .sortBy(_.getDistance(unit))
          .head

      waitFor(waitLatency).map { _ =>
        unit.gather(closestMineral, false);
      }
    }
  }

  def nextExpoLocation(builder: BWUnit): TilePosition = {
    BWTA.getBaseLocations.toList
      .map(_.getTilePosition)
      .filter(loc => game.canBuildHere(builder, loc, Zerg_Hatchery))
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

