package jaidong.utils

case class RollingAverage(windowSize: Int, data: () => Int) {
  private var valueLastFrame = 0
  private var lastUpdate = 0
  private val rolling = new collection.mutable.Queue[(Int, Int)]()
  private var rollingValue = 50
  private var rollingTime = 0

  def spend(amount: Int) = {
    valueLastFrame -= amount
  }

  def rate: Float =
    try { rollingValue.toFloat / rollingTime }
    catch { case _: Throwable => 0f }

  def update(frame: Int) = {
    val value = data()

    if (value > valueLastFrame) {
      val diff = value - valueLastFrame
      val frameDiff = frame - lastUpdate

      rollingValue += diff
      rollingTime += frameDiff

      rolling.enqueue(diff -> frameDiff)
      if (rolling.length > windowSize) {
        val dequeued = rolling.dequeue()
        rollingValue -= dequeued._1
        rollingTime -= dequeued._2
      }

      lastUpdate = frame
    }

    valueLastFrame = value
  }

  def estimateUntil(amount: Int): Int =
    math.max(0, ((amount - data()) / rate).toInt)
}
