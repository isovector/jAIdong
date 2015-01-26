package jaidong

import bwapi.UnitType.{None => Unknown_None, _}
import bwapi.{Unit => BWUnit, _}
import bwta.BaseLocation
import bwta.BWTA
import scala.collection.JavaConversions._
import scala.concurrent.Future
import scala.util.{Success, Failure}

import jaidong.Implicits._
import jaidong.util._


object Bot extends Prediction with Allocation with BWFutures {
  val mirror = new Mirror()
  lazy val game: Game = mirror.getGame
  lazy val self: Player = game.self
}


class TestBot extends DefaultBWListener {
  def game = Bot.game
  def self = Bot.self
  var nextExpo: Option[TilePosition] = None
  val macromgr = new jaidong.mgr.MacroMgr()

  def run() = {
    Bot.mirror.getModule.setEventListener(this)
    Bot.mirror.startGame()
  }

  override def onStart(): Unit = {
    game.enableFlag(1)
    game.setLocalSpeed(0)

    BWTA.readMap();
    BWTA.analyze();
  }

  override def onFrame(): Unit = {
    Bot.runPromises()
    Bot.updatePrediction()

    if (game.getKeyState(Key.K_DELETE)) {
      Bot.debugFutures()
    }

    game.setTextSize(10);
    game.drawTextScreen(10, 10, "Available: " + Bot.available.toString)

    macromgr.bases.foreach { base =>
      val hatch = base.hatch.getPosition
      val dir = base.relMineralDir
      val gdir = base.relGasDir

      game.drawLineMap(hatch.getX, hatch.getY, hatch.getX + dir._1 * 128, hatch.getY + dir._2 * 128, bwapi.Color.Blue)
      game.drawLineMap(hatch.getX, hatch.getY, hatch.getX + gdir._1 * 128, hatch.getY + gdir._2 * 128, bwapi.Color.Green)
    }


    if (nextExpo.isDefined) {
      val tpos = nextExpo.get
      val pos = tpos.toPosition
      game.drawBoxMap(pos.getX, pos.getY, pos.getX + 4 * 32, pos.getY + 3 * 32, bwapi.Color.Orange)
    }

    self.getUnits.foreach { unit =>
      if (unit.getType.isWorker && self.minerals >= 150 && nextExpo.isEmpty && macromgr.bases.length < 3) {
        nextExpo = Some(nextExpoLocation(unit))
        val tpos = nextExpo.get
        val pos = tpos.toPosition

        val utype = Zerg_Hatchery
        var voucherFuture: Option[Future[Voucher]] = None
        Bot.synchronize(
          unit.estimateTimeTo(tpos),
          Bot.mineralRate.estimateUntil(utype.cost.minerals)
        ).flatMap { _ =>
          println("moving")

          if (macromgr.bases.length == 1)
            voucherFuture = Some(utype.allocate(80))
          Bot.moveTo(unit, pos)
        }.flatMap { _ =>
          println("made it here")
          game.setScreenPosition(pos.getX - 640/2, pos.getY - 330/2)

          if (macromgr.bases.length != 1)
            voucherFuture = Some(utype.allocate(50))
          Bot.waitFor(voucherFuture.get)
        }.map { voucher =>
          println("building hatchery")
          voucher.forNext(utype)
          unit.build(tpos, utype)
        }.onFailure {
          case _: Exception =>
            nextExpo = None
            onUnitMorph(unit)
        }
      }
    }
  }

  override def onUnitComplete(unit: BWUnit): Unit = {
    Bot.runCreatePromises(unit, CreateSource.COMPLETE)

    if (game.getFrameCount < Bot.waitLatency) {
      onUnitMorph(unit)
    }

    if (unit is Zerg_Larva) {
      macromgr.onNewLarva(unit)
    }
  }

  override def onUnitMorph(unit: BWUnit): Unit = {
    Bot.runCreatePromises(unit, CreateSource.MORPH)

    if (unit is Zerg_Hatchery) {
      macromgr.onNewHatchery(unit)
      nextExpo = None
    }

    if (unit.getType.isWorker) {
      val closestMineral =
        game.neutral.getUnits.toList
          .filter(_.getType.isMineralField)
          .sortBy(_.getDistance(unit))
          .head

      Bot.sleep(Bot.waitLatency).map { _ =>
        unit.gather(closestMineral, false);
      }
    }

    if (unit is Zerg_Overlord) {
      Bot.sleep(Bot.waitLatency).map { _ =>
        unit.move(
          BWTA.getStartLocations
            .map(_.getTilePosition)
            .filter(_ != self.getStartLocation)
            .head
            .toPosition)
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

  override def onReceiveText(player: Player, msg: String): Unit = {
    println(msg)
  }
}


object Main {
  def main(args: Array[String]): Unit = {
    System.setProperty("os.arch", "x86")
    new TestBot().run()
  }
}

