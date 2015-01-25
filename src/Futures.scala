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

  def priorityOf[T](obj: PrioObj[T]) =
    obj.priority
  implicit val ordering = Ordering.by(priorityOf[Unit])

  case class PrioObj[T](priority: Int, predicate: () => Boolean) {
    val promise = Promise[T]()
  }

  private object promises {
    var times = List[PredObj]()
    val moves =
      new collection.mutable.HashMap[BWUnit, MoveObj]()
    val priority =
      new collection.mutable.PriorityQueue[PrioObj[Unit]]()
  }

  def waitLatency: Int = game.getLatencyFrames + 25

  private def getFirstFrame: Int =
    game.getFrameCount + waitLatency


  def moveTo(unit: BWUnit, pos: Position): Future[Position] = {
    val obj = MoveObj(getFirstFrame)
    promises.moves += unit -> obj
    unit.move(pos)

    obj.promise.future
  }

  def sleep(duration: Int): Future[Unit] = {
    val when = game.getFrameCount + duration
    val obj = new PredObj(() => game.getFrameCount >= when)
    promises.times = promises.times :+ obj
    obj.promise.future
  }

  def waitFor(prio: Int, predicate: () => Boolean): Future[Unit] = {
    val obj = new PrioObj[Unit](
      prio,
      predicate)
    promises.priority.enqueue(obj)
    obj.promise.future
  }

  def waitFor[T](future: Future[T]): Future[T] = {
    val obj = new PredObj(() => future.isCompleted)
    promises.times = promises.times :+ obj
    obj.promise.future.flatMap { _ =>
      future
    }
  }

  def runPromises() = {
    val now = game.getFrameCount

    if (!promises.priority.isEmpty) {
      while (promises.priority.max.predicate()) {
        val obj = promises.priority.dequeue()
        obj.promise.success({})
      }
    }

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

