import bwapi.{Unit => BWUnit, _}
import bwta.BWTA
import scala.collection.JavaConversions._


class TestBot extends DefaultBWListener {
  val mirror = new Mirror()
  lazy val game: Game = mirror.getGame
  lazy val self: Player = game.self

  def run() = {
    mirror.getModule.setEventListener(this)
    mirror.startGame()
  }

  override def onStart(): Unit = {
    game.enableFlag(1)
    game.setLocalSpeed(10)
  }

  override def onFrame(): Unit = {
    game.setTextSize(10);
    game.drawTextScreen(10, 10, "Playing as " + self.getName + " - " + self.getRace);

    self.getUnits.foreach { unit =>
      if (unit.getType == UnitType.Terran_Command_Center && self.minerals >= 50) {
        unit.train(UnitType.Terran_SCV);
      }

      if (unit.getType.isWorker && unit.isIdle) {
        val closestMineral =
          game.neutral.getUnits.toList
            .filter(_.getType.isMineralField)
            .sortBy(_.getDistance(unit))
            .head

          unit.gather(closestMineral, false);
      }
    }
  }

  override def onEnd(won: Boolean): Unit = {
    System.exit(0)
  }

  override def onPlayerLeft(player: Player): Unit = {
    System.exit(0)
  }
}


object CompilerMain {
  def main(args: Array[String]): Unit = {
    System.setProperty("os.arch", "x86")
    println(System.getProperty("os.arch"))

    new TestBot().run()
  }
}

/*
public class TestBot1 extends DefaultBWListener {

    @Override
    public void onFrame() {


        //iterate through my units
        for (Unit myUnit : self.getUnits()) {
            units.append(myUnit.getType()).append(" ").append(myUnit.getTilePosition()).append("\n");

            //if there's enough minerals, train an SCV
            if (myUnit.getType() == UnitType.Terran_Command_Center && self.minerals() >= 50) {
                myUnit.train(UnitType.Terran_SCV);
            }

            //if it's a drone and it's idle, send it to the closest mineral patch
            if (myUnit.getType().isWorker() && myUnit.isIdle()) {
                Unit closestMineral = null;

                //find the closest mineral
                for (Unit neutralUnit : game.neutral().getUnits()) {
                    if (neutralUnit.getType().isMineralField()) {
                        if (closestMineral == null || myUnit.getDistance(neutralUnit) < myUnit.getDistance(closestMineral)) {
                            closestMineral = neutralUnit;
                        }
                    }
                }

                //if a mineral patch was found, send the drone to gather it
                if (closestMineral != null) {
                    myUnit.gather(closestMineral, false);
                }
            }
        }

        //draw my units on screen
        game.drawTextScreen(10, 25, units.toString());
    }

    public static void main(String[] args) {
        new TestBot1().run();
    }
}*/
