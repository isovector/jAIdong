package jaidong

import bwapi.{Unit => BWUnit, _}
import jaidong.Implicits._
import jaidong.util._
import scala.collection.JavaConversions._
import scala.concurrent.Future
import scala.util.{Success, Failure}

case class Resources(minerals: Int, gas: Int) {
  def +(b: Resources) =
    Resources(minerals + b.minerals, gas + b.gas)

  def -(b: Resources) =
    Resources(minerals - b.minerals, gas - b.gas)

  def isPositive =
    minerals >= 0 && gas >= 0

  override def toString() =
    (if (isPositive) "\u0007" else "\u0008") + s"$minerals/$gas"

  def >(b: Resources) =
    minerals > b.minerals && gas > b.gas
}

trait Allocation extends BWFutures {
  val self: Player

  def rawResources: Resources =
    new Resources(self.minerals, self.gas)

  private object allocated {
    var minerals: Int = 0
    var gas: Int = 0

    def asResources = Resources(minerals, gas)
  }

  def available: Resources =
    rawResources - allocated.asResources

  case class Voucher(resources: Resources) {
    def cash() = {
      sleep(waitLatency).map { _ =>
        cancel()
      }
    }

    def cancel() = {
      allocated.minerals -= resources.minerals
      allocated.gas -= resources.gas
    }
  }

  def allocate(priority: Int, amount: Resources): Future[Voucher] = {
    allocated.minerals += amount.minerals
    allocated.gas += amount.gas

    waitFor(priority, () => available.isPositive).map { _ =>
      Voucher(amount)
    }
  }
}
