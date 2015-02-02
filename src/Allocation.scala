package jaidong

import bwapi.{Unit => BWUnit, _}
import jaidong.Implicits._
import jaidong.util._
import scala.collection.JavaConversions._
import scala.concurrent.Future
import scala.util.{Success, Failure}
import shapeless.contrib.spire._
import spire.implicits._

case class Resources(minerals: Int, gas: Int, supply: Int) {
  override def toString() = s"\u0007$minerals/$gas/$supply"
}

case class Voucher(resources: Resources, onCash: () => Unit) {
  def cash() = {
    onCash()
  }

  def forNext(utype: UnitType) = {
    Bot.onCreate(utype, CreateSource.ANY).map { _ =>
      cash()
    }
  }
}

trait Allocation {
  val self: Player

  def available: Resources =
    Resources(self.minerals, self.gas, self.supplyTotal - self.supplyUsed)

  def allocate(priority: Int, amount: Resources): Future[Voucher] = {
    Bot.waitFor(priority, () => available >= amount, false).map { obj =>
      Voucher(amount, obj.toDequeue.get)
    }
  }
}
