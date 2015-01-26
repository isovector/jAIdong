package jaidong

import bwapi.{Unit => BWUnit, _}
import bwta.BWTA
import scala.concurrent.Future

package object Implicits {
  private def clamp(x: Int, bot: Int, top: Int) = {
    math.max(math.min(x, top), bot)
  }

  case class RichPosition(u: Position) {
    def toTilePosition: TilePosition =
      new TilePosition(u.getX / 32, u.getY / 32)

    def compare(other: Position, epsilon: Int = 3): (Int, Int) = {
      toTilePosition.compare(other.toTilePosition, epsilon)
    }
  }

  case class RichTilePosition(u: TilePosition) {
    def toPosition: Position =
      new Position(u.getX * 32, u.getY * 32)

    def compare(other: TilePosition, epsilon: Int = 3): (Int, Int) = {
      val dx = (u.getX - other.getX) / epsilon
      val dy = (u.getY - other.getY) / epsilon

      (clamp(dx, -1, 1), clamp(dy, -1, 1))
    }
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

