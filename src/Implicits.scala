package jaidong

import bwapi.{Unit => BWUnit, _}
import bwta.BWTA

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

  implicit def toRichPos(u: Position): RichPosition =
    new RichPosition(u)

  implicit def toRichTilePos(u: TilePosition): RichTilePosition =
    new RichTilePosition(u)

  implicit def toRichUnit(u: BWUnit): RichUnit =
    new RichUnit(u)
}

