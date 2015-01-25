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

  override def toString() = s"\u0007$minerals/$gas"

  def >=(b: Resources) =
    minerals >= b.minerals && gas >= b.gas
}

trait Allocation extends BWFutures {
  val self: Player

  def available: Resources =
    new Resources(self.minerals, self.gas)

  case class Voucher(resources: Resources, onCancel: () => Unit) {
    def cash() = {
      sleep(waitLatency).map { _ =>
        cancel()
      }
    }

    def cancel() = {
      onCancel()
    }
  }

  def allocate(priority: Int, amount: Resources): Future[Voucher] = {
    waitFor(priority, () => available >= amount, false).map { obj =>
      Voucher(amount, obj.toDequeue.get)
    }
  }
}
