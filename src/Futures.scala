package jaidong

import bwapi.{Unit => BWUnit, _}
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

  private case class MoveObj(after: Int) {
    val promise = Promise[Position]()
  }

  private object promises {
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

  def promiseMoves() = {
    val now = game.getFrameCount

    promises.moves.foreach { case (unit, obj) =>
      if (!unit.exists) {
        obj.promise.failure(new Exception("unit lost"))
        promises.moves -= unit
      } else if (unit.isIdle && now > obj.after) {
        obj.promise.success(unit.getPosition)
        promises.moves -= unit
      }
    }
  }
}
}

