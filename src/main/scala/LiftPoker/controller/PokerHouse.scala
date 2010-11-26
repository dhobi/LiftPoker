package LiftPoker.controller

import net.liftweb.actor.LiftActor

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 08.07.2010
 * Time: 07:08:04
 * To change this template use File | Settings | File Templates.
 */

object PokerHouse {
  private lazy val tables: List[Table] = List(
    Table.table,
    TableTwo.table,
    TableThree.table,
    TableFour.table)

  def getTables = {
    tables
  }

}