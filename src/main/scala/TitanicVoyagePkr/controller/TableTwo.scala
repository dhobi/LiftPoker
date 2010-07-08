package TitanicVoyagePkr.controller

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 08.07.2010
 * Time: 07:05:30
 * To change this template use File | Settings | File Templates.
 */

class TableTwo extends Table {
  override protected val id = 2
  override protected val size = 2
  override protected val name = "Lift Poker 2"
}

object TableTwo {
  lazy val table = new TableTwo
}

////////////////////

class TableThree extends Table {
  override protected val id = 3
  override protected val size = 4
  override protected val name = "Lift Poker 3"
  override protected val smallblind = 5
  override protected val bigblind = 10
  override protected val raiseAmount = 10
}

object TableThree {
  lazy val table = new TableThree
}