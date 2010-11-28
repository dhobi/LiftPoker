package LiftPoker.controller

import LiftPoker.comet.Bot

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
  override protected val name = "Bots Hell"
  override protected val smallblind = 5
  override protected val bigblind = 10
  override protected val raiseAmount = 10
  val bot0 = new Bot(this,1, "Alex")
  val bot1 = new Bot(this,2, "Sandra")
  val bot2 = new Bot(this,3, "Fritz")
}

object TableThree {
  lazy val table = new TableThree
}

////////////////////

class TableFour extends Table {
  override protected val id = 4
  override protected val size = 8
  override protected val name = "Titanic Voyage"
  override protected val smallblind = 1
  override protected val bigblind = 2
  override protected val raiseAmount = 4
}

object TableFour {
  lazy val table = new TableFour
}