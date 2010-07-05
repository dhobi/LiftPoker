package TitanicVoyagePkr.model

import TitanicVoyagePkr.controller.ResetGame

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 28.06.2010
 * Time: 17:10:46
 * To change this template use File | Settings | File Templates.
 */

class Poker {

  var activeRound = 0
  private var round : Map[Int,Any] = Map(0->Preflop(),1->Flop(),2->Turn(),3->River(),4->ShowDown(),5->ResetGame())

  def next() = {
    activeRound = (activeRound + 1) % round.size
  }

  def getActiveRound = {
    round.get(activeRound).get
  }
}

case class Preflop()
case class Flop()
case class Turn()
case class River()
case class ShowDown()