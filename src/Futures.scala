package jaidong

import bwapi.{Unit => BWUnit, _}
import bwta.BWTA
import java.util.concurrent.Executors
import scala.collection.JavaConversions._
import scala.concurrent.{Future, Promise, ExecutionContext}

package object util {

// Run futures in the same thread
implicit val ec = new ExecutionContext {
  def execute(runnable: Runnable) {
    runnable.run()
  }

  def reportFailure(t: Throwable) {}
}

trait BWFutures {
  val game: Game
  val self: Player

  private case class MoveObj(after: Int) {
    val promise = Promise[Position]()
  }

  private case class PredObj(predicate: () => Boolean) {
    val promise = Promise[Unit]()
  }

  private object promises {
    var times = List[PredObj]()
    val moves =
      new collection.mutable.HashMap[BWUnit, MoveObj]()
  }

  private def getFirstFrame: Int =
    game.getFrameCount + game.getLatencyFrames + 25

  def moveTo(unit: BWUnit, pos: Position): Future[Position] = {
    unit.move(pos)

    val obj = MoveObj(getFirstFrame)
    promises.moves += unit -> obj

    obj.promise.future
  }

  def waitFor(duration: Int): Future[Unit] = {
    val when = game.getFrameCount + duration
    val obj = new PredObj(() => game.getFrameCount >= when)
    promises.times = promises.times :+ obj
    obj.promise.future
  }

  def haveEnough(minerals: Int, gas: Int): Future[Unit] = {
    val obj = new PredObj(() => self.minerals >= minerals && self.gas >= gas)
    promises.times = promises.times :+ obj
    obj.promise.future
  }

  def runPromises() = {
    val now = game.getFrameCount

    promises.times = promises.times.filter { obj =>
      if (obj.predicate()) {
        obj.promise.success({})
        false
      } else true
    }

    promises.moves.foreach { case (unit, obj) =>
      if (!unit.exists) {
        obj.promise.failure(new Exception("unit lost"))
        promises.moves -= unit
      } else if (unit.isIdle && now >= obj.after) {
        obj.promise.success(unit.getPosition)
        promises.moves -= unit
      }
    }
  }
}
}

