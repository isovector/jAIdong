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

  def priorityOf(obj: PrioObj) =
    obj.priority
  implicit val ordering = Ordering.by(priorityOf)

  case class PrioObj(
      priority: Int,
      predicate: () => Boolean,
      dequeue: Boolean) {
    var toDequeue: Option[() => Unit] = None
    val promise = Promise[PrioObj]()
  }

  private object promises {
    var times = List[PredObj]()
    val moves =
      new collection.mutable.HashMap[BWUnit, MoveObj]()
    val priority =
      new collection.mutable.PriorityQueue[PrioObj]()
  }

  def waitLatency: Int = game.getLatencyFrames + 25

  private def getFirstFrame: Int =
    game.getFrameCount + waitLatency


  def moveTo(unit: BWUnit, pos: Position): Future[Position] = {
    unit.move(pos)

    val obj = MoveObj(getFirstFrame)
    promises.moves += unit -> obj

    obj.promise.future
  }

  def sleep(duration: Int): Future[Unit] = {
    val when = game.getFrameCount + duration
    val obj = new PredObj(() => game.getFrameCount >= when)
    promises.times = promises.times :+ obj
    obj.promise.future
  }

  def waitFor(
      prio: Int,
      predicate: () => Boolean,
      dequeue: Boolean = true): Future[PrioObj] = {
    val obj = new PrioObj(
      prio,
      predicate,
      dequeue)
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
      val max = promises.priority.max

      if (max.toDequeue.isDefined) {
      } else if (max.predicate()) {
        if (max.dequeue)
          promises.priority.dequeue()
        else
          max.toDequeue = Some(() => promises.priority.dequeue())

        max.promise.success(max)
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

