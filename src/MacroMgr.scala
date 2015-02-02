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
    else (dir.x > 0)
      pos += Vec2(hatchSize.x, 0)

    if (dir.y < 0)
      pos -= Vec2(0, size.y)
    else (dir.y > 0)
      pos += Vec2(0, hatchSize.y)

    pos
  }
}

class MacroMgr {
  def game = Bot.game
  def self = Bot.self

  lazy val main =
    Base(
      self.getUnits.filter(_ is Zerg_Hatchery).head,
      BWTA
        .getStartLocations
        .filter(_.getTilePosition == self.getStartLocation)
        .head)
  val bases = new collection.mutable.MutableList[Base]()
  val hatches = new collection.mutable.MutableList[BWUnit]()

  var supplyIncoming: Int = 0

  def needsSupply: Boolean = {
    Bot.available.supply + supplyIncoming <= 2
  }

  def onNewHatchery(hatch: BWUnit) = {
    hatches += hatch

    val baseLocation =
      BWTA.getBaseLocations
        .find(_.getTilePosition == hatch.getTilePosition)
    if (baseLocation.isDefined)
      bases += Base(hatch, baseLocation.get)
  }

  def getLarva: BWUnit =
    self.getUnits.toList.filter(_ is Zerg_Larva).head

  def onNewLarva(larva: BWUnit) = {
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
}

