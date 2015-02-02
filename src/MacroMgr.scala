package jaidong.mgr

import bwapi.UnitType.{None => Unknown_None, _}
import bwapi.{Unit => BWUnit, _}
import bwta.BaseLocation
import bwta.BWTA
import scala.collection.JavaConversions._
import scala.concurrent.Future
import scala.util.{Success, Failure}

import shapeless.contrib.spire._
import spire.implicits._

import jaidong._
import jaidong.Implicits._
import jaidong.util._

case class Base(hatch: BWUnit, location: BaseLocation) {
  val minerals =
    Bot.game.getUnitsInRadius(location.getPosition, 6 * 64)
      .toList
      .filter(_.getType.isMineralField)
  val geysers =
    Bot.game.neutral.getUnits.toList
      .filter(_ is Resource_Vespene_Geyser)
      .filter(_.getPosition.getApproxDistance(location.getPosition) < 6 * 64)
  private val totalMinPos =
    minerals
      .map(_.getPosition)
      .reduceLeft((prev, next) =>
        new Position(prev.getX + next.getX, prev.getY + next.getY))
  val mineralCluster =
    new Position(
      totalMinPos.getX / minerals.length,
      totalMinPos.getY / minerals.length)

  val relMineralDir = mineralCluster.compare(hatch.getPosition)
  val relGasDir     =
    if (geysers.length > 0)
      geysers.head.getPosition.compare(hatch.getPosition)
    else Vec2.zero

  val drones = new collection.mutable.MutableList[BWUnit]()

  def getPositionFor(ut: UnitType): TilePosition = {
    val size = ut.tileSize
    val hatchSize = hatch.getType.tileSize
    val dir = -relMineralDir

    var pos: Vec2 = hatch.getTilePosition

    if (dir.x < 0)
      pos -= Vec2(size.x, 0)
    else if (dir.x > 0)
      pos += Vec2(hatchSize.x, 0)

    if (dir.y < 0)
      pos -= Vec2(0, size.y)
    else if (dir.y > 0)
      pos += Vec2(0, hatchSize.y)

    pos
  }
}

class MacroMgr {
  def game = Bot.game
  def self = Bot.self

  val bases = new collection.mutable.MutableList[Base]()
  def main = bases.head
  val hatches = new collection.mutable.MutableList[BWUnit]()

  val inProgress = collection.mutable.Set[UnitType]()
  val has = collection.mutable.Set[UnitType]()

  val reservedSpots =
    new collection.mutable.MutableList[(TilePosition, Vec2)]()

  var supplyIncoming: Int = 0

  def needsSupply: Boolean = {
    Bot.available.supply + supplyIncoming <= 2
  }

  def onNewHatchery(hatch: BWUnit) = {
    reservedSpots += ((hatch.getTilePosition, hatch.getType.tileSize))

    hatches += hatch

    val baseLocation =
      BWTA.getBaseLocations
        .find(_.getTilePosition == hatch.getTilePosition)
    if (baseLocation.isDefined)
      bases += Base(hatch, baseLocation.get)

    hatch.getLarva.toList.foreach(onNewLarva)
  }

  def getLarva: BWUnit =
    self.getUnits.toList.filter(_ is Zerg_Larva).head

  def onNewLarva(larva: BWUnit) = {
    println(main.relMineralDir)
    if (main.relMineralDir.x < 0) {
      Bot.sleep(Bot.waitLatency).map { _ =>
        println("larva stop")
        larva.stop()
      }
    }

    if (needsSupply) {
      val extraSupply = Zerg_Overlord.supplyProvided
      supplyIncoming += extraSupply

      Zerg_Overlord.allocate(90).flatMap { voucher =>
        voucher.forNext(Zerg_Egg)
        Bot.morph(getLarva, Zerg_Overlord)
      }.map { ovie =>
        supplyIncoming -= extraSupply
      }
    } else {
      Zerg_Drone.allocate(50).map { voucher =>
        voucher.forNext(Zerg_Egg)
        getLarva.morph(Zerg_Drone)
      }
    }
  }

  def getBaseFor(utype: UnitType): Base = {
    main
  }

  def getAvailableWorker(near: Position): BWUnit = {
    Bot.game
      .getUnitsInRadius(near, 10 * 64)
      .toList
      .filter(_.getType.isWorker)
      .sortBy(_.getDistance(near))
      .head
  }

  def needs(utype: UnitType): Boolean = {
    !(has.contains(utype) || inProgress.contains(utype))
  }

  def build(utype: UnitType, priority: Int): Unit = {
    if (inProgress.contains(utype)) {
      return
    }

    val base = getBaseFor(utype)
    val pos = base.getPositionFor(utype)

    reservedSpots += ((pos, utype.tileSize))

    inProgress += utype

    utype.allocate(priority).flatMap{ voucher =>
      voucher.forNext(utype)
      getAvailableWorker(pos.toPosition).build(pos, utype)
      Bot.onCreate(utype, CreateSource.ANY)
    }
    .onComplete {
      case Success(_) =>
        has += utype
        inProgress -= utype
      case Failure(_) =>
        inProgress -= utype
    }
  }
}
