package jaidong

import bwapi.{Unit => BWUnit, _}
import bwta.BWTA
import scala.concurrent.Future

package object Implicits {
  case class RichPosition(u: Position) {
    def toTilePosition: TilePosition =
      new TilePosition(u.getX / 32, u.getY / 32)
  }

  case class RichTilePosition(u: TilePosition) {
    def toPosition: Position =
      new Position(u.getX * 32, u.getY * 32)
  }

  case class RichUnit(u: BWUnit) extends AnyVal {
    def is(ut: UnitType) = u.getType == ut

    def estimateTimeTo(pos: TilePosition) = {
      val dist = BWTA.getGroundDistance(u.getTilePosition, pos)
      (dist / u.getType.topSpeed).toInt
    }
  }

  case class RichUnitType(u: UnitType) {
    def allocate(priority: Int): Future[Voucher] = {
      println("allocating: " + u.toString)
      Bot.allocate(priority, cost)
    }

    def cost = {
      Resources(
        u.mineralPrice,
        u.gasPrice,
        u.supplyRequired)
    }
  }

  implicit def toRichPos(u: Position): RichPosition =
    new RichPosition(u)

  implicit def toRichTilePos(u: TilePosition): RichTilePosition =
    new RichTilePosition(u)

  implicit def toRichUnit(u: BWUnit): RichUnit =
    new RichUnit(u)

  implicit def toRichUnitType(u: UnitType): RichUnitType =
    new RichUnitType(u)
}

