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

object CreateSource {
  type Type = Int

  val CREATE   = 1 << 0
  val COMPLETE = 1 << 1
  val MORPH    = 1 << 2
  val ANY      = CREATE | COMPLETE | MORPH
}

trait BWFutures {
  val game: Game
  val self: Player

  private case class CreateObj(utype: UnitType, source: CreateSource.Type) {
    val promise = Promise[BWUnit]()
  }

  private case class MoveObj(after: Int, pos: Position) {
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
    var creates = collection.mutable.ListBuffer[CreateObj]()
    val priority =
      new collection.mutable.PriorityQueue[PrioObj]()
  }

  def waitLatency: Int = game.getLatencyFrames + 25

  private def getFirstFrame: Int =
    game.getFrameCount + waitLatency


  def moveTo(unit: BWUnit, pos: Position): Future[Position] = {
    unit.move(pos)

    val obj = MoveObj(getFirstFrame, pos)
    promises.moves += unit -> obj

    obj.promise.future
  }

  def build(unit: BWUnit, utype: UnitType, pos: TilePosition): Future[BWUnit] = {
    unit.build(pos, utype)

    val obj = CreateObj(utype, CreateSource.ANY)
    promises.creates += obj

    sleep(waitLatency * 2).map { _ =>
      obj.promise.failure(new Exception("building didn't appear"))
    }

    obj.promise.future.onFailure {
      case _ =>
        promises.creates -= obj
    }

    obj.promise.future

  }

  def morph(unit: BWUnit, utype: UnitType): Future[BWUnit] = {
    unit.morph(utype)
    onCreate(utype, CreateSource.MORPH)
  }

  def onCreate(utype: UnitType, source: CreateSource.Type): Future[BWUnit] = {
    val obj = CreateObj(utype, source)
    promises.creates += obj

    obj.promise.future.onFailure {
      case _ =>
        promises.creates -= obj
    }

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

  def synchronize(event1: Int, event2: Int) = {
    val diff =
      if (event1 == 0 || event2 == 0)
        0
      else
        math.max(event1, event2) - math.min(event1, event2)
    sleep(diff)
  }

  def runCreatePromises(unit: BWUnit, flag: CreateSource.Type) = {
    promises
      .creates
      .filter(obj => obj.utype == unit.getType && (obj.source & flag) == flag)
      .foreach { obj =>
        promises.creates -= obj
        obj.promise.success(unit)
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
        val dist = obj.pos.getDistance(unit.getPosition)

        if (dist < 10) {
          obj.promise.success(unit.getPosition)
        } else {
          obj.promise.failure(new Exception("too far"))
        }

        promises.moves -= unit
      }
    }
  }

  def debugFutures() = {
    println()
    println()
    println("----- TIMES -----")
    println(promises.times.mkString("\n"))

    println()
    println()
    println("----- PRIORITY -----")
    println(promises.priority.mkString("\n"))

    println()
    println()
    println("----- CREATES -----")
    println(promises.creates.mkString("\n"))

    println()
    println()
    println("----- MOVES -----")
    println(promises.moves.mkString("\n"))
  }
}
}

