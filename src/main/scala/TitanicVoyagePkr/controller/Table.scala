package TitanicVoyagePkr.controller

import net.liftweb.common.SimpleActor
import net.liftweb.actor.LiftActor
import TitanicVoyagePkr.comet.Player
import net.liftweb.util.ActorPing
import net.liftweb.util.Helpers._
import TitanicVoyagePkr.model._
import net.liftweb.http.{TemplateFinder, S}
import xml.Text

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 23.06.2010
 * Time: 11:33:50
 * To change this template use File | Settings | File Templates.
 */

class Table extends LiftActor {
  protected val id = 1
  protected val name = "Lift Poker"

  protected var tablewatchers: List[Player] = List()
  protected var tableplayers: Map[int, Player] = Map()
  protected var interrupted = true
  protected var money: List[Money] = List()
  protected var roundmoney: Map[Player, Money] = Map()
  protected var cardstack = new CardStack()
  protected val dealer = new Dealer(this)
  protected val size = 4
  protected val tobind = TemplateFinder.findAnyTemplate("tables/table" + size :: Nil)


  var flop: List[Card] = List()
  var turn: List[Card] = List()
  var river: List[Card] = List()

  protected val smallblind = 1
  protected val bigblind = 2
  protected val raiseAmount = 4

  def getId = {
    id
  }

  def getPlayers = {
    tableplayers
  }

  def getSize = {
    size
  }

  def getName = {
    name
  }

  def getXML = {
    tobind.openOr(Text("Not found"))
  }

  def getBigBlind = {
    bigblind
  }

  def getSmallBlind = {
    smallblind
  }

  def getRaiseAmount = {
    raiseAmount
  }

  protected def messageHandler =
    {
      case AddWatcher(me) => {
        if (!tablewatchers.find(p => p.id == me.id).isDefined) {
          tablewatchers = tablewatchers ::: List(me)
        }
      }
      case AddPlayerToTable(me, seat) => {
        if (!tableplayers.isDefinedAt(seat)) {
          me.id = seat
          tableplayers ++= List(me.id -> me)
          updatePlayers

          //Tell dealer
          dealer.addUser(me)
        }
      }
      case RemoveWatcher(me) => {
        tablewatchers = tablewatchers.filter(p => p.id != me.id)

      }
      case RemovePlayer(me) => {
         tableplayers -= me.id

        //Tell dealer
        dealer.removeUser(me)
        me.id = 0
        me.resetGame

        updatePlayers
      }
      case Countdown() => {
        ActorPing.schedule(this, Seconds(5), 1000L)
        interrupted = false
      }
      case Seconds(s: int) => {
        if (!interrupted) {

          if (s != 0) {
            ActorPing.schedule(this, Seconds(s - 1), 1000L)
            tableplayers.values.foreach(_ ! Seconds(s))
          } else {
            tableplayers.values.foreach(_ ! Seconds(s))
            dealer.dealCards
          }
        }
      }
      case DealCards() => {
        tableplayers.values.foreach(player => player.addCards(cardstack.getCards(2)))
        updateCards
      }
      case SmallBlind(player: Player) => {
        if (!interrupted) {
          money = money ::: List(new Money(smallblind))
          roundmoney ++= List(player -> new Money(smallblind))

          dealer.updateMoney(player, smallblind)

          tablewatchers.foreach(_ ! PutSmallBlind(player, smallblind))
        }
      }
      case BigBlind(player: Player) => {
        if (!interrupted) {
          money = money ::: List(new Money(bigblind))
          roundmoney ++= List(player -> new Money(bigblind))

          dealer.updateMoney(player, bigblind)

          tablewatchers.foreach(_ ! PutBigBlind(player, bigblind))
        }
      }
      case SetMoney(player: Player, money: int) => {

        dealer.updateMoney(player, money)

        this.money = this.money ::: List(new Money(money))
        roundmoney ++= List(player -> new Money(player.usedMoney))

        tablewatchers.foreach(_ ! SetMoney(player, player.usedMoney))
      }
      case WaitForAction(players: List[Player], player: Player) => {
        if(!interrupted) {
          tablewatchers.foreach(_ ! PutWait(players, player))
        }
      }
      case Fold(player: Player) => {
        tablewatchers.foreach(_ ! StopCountdown())
        dealer.fold(player)
        tablewatchers.foreach(_ ! Fold(player))

      }
      case Call(player: Player) => {
        tablewatchers.foreach(_ ! StopCountdown())
        dealer.call(player)
      }
      case Check(player: Player) => {
        tablewatchers.foreach(_ ! StopCountdown())
        dealer.check(player)
      }
      case Raise(player: Player) => {
        tablewatchers.foreach(_ ! StopCountdown())
        dealer.raise(player)
      }
      case Flop() => {
        roundmoney = Map()
        tableplayers.values.foreach(p => p.usedMoney = 0)

        flop = cardstack.getCards(3)
        tablewatchers.foreach(_ ! ShowFlop(flop))
        sendStatusMessage("Flop: " + flop.map(c => c.getJavaCard.toString).foldRight[String]("")(_ + "," + _))
        updateAllMoney

        dealer.waitingForAction
      }
      case Turn() => {
        roundmoney = Map()
        tableplayers.values.foreach(p => p.usedMoney = 0)

        turn = cardstack.getCards(1)
        tablewatchers.foreach(_ ! ShowTurn(turn))
        sendStatusMessage("Turn: " + turn.firstOption.get.getJavaCard.toString)
        updateAllMoney
        dealer.waitingForAction
      }
      case River() => {
        roundmoney = Map()
        tableplayers.values.foreach(p => p.usedMoney = 0)

        river = cardstack.getCards(1)
        tablewatchers.foreach(_ ! ShowRiver(river))
        sendStatusMessage("River: " + river.firstOption.get.getJavaCard.toString)
        updateAllMoney
        dealer.waitingForAction
      }
      case ShowDown() => {
        var winner = dealer.showdown
        winner.money.add(allPlayedMoney)
        tablewatchers.foreach(_ ! ShowShowDown(tableplayers.values.toList, winner))
        ActorPing.schedule(this, ResetGame(), 10000L)
      }
      case ResetGame() => {
        resetGame
      }
      case SendMessage(player: String, str: String) => {
        sendMessage(player, str)
      }
      case _ =>
    }

  def updatePlayers = {
    tablewatchers.foreach(_ ! AddPlayers(tableplayers.values.toList))
  }

  def updateCards = {
    tablewatchers.foreach(_ ! CardsDealed(tableplayers.values.toList))
  }

  def updateAllMoney = {
    tablewatchers.foreach(_ ! ShowAllMoney(tableplayers.values.toList, allPlayedMoney))
  }

  def sendMessage(player: String, str: String) = {
    tablewatchers.foreach(_ ! ShowMessage(player, str))
  }

  def sendStatusMessage(message: String) = {
    tablewatchers.foreach(_ ! ShowStatusMessage(message))
  }

  def getHighestRoundMoney = {
    roundmoney.values.toList.sort(_.get > _.get).firstOption.getOrElse(new Money(0))
  }

  def allPlayedMoney = {
    money.foldLeft[Money](new Money(0))(_ + _).get
  }

  def resetGame() = {
    interrupted = true
    flop = List()
    turn = null
    river = null
    cardstack = new CardStack
    roundmoney = Map()
    money = List()
    dealer.reset
    tablewatchers.foreach(_ ! ResetGame())
  }

}

case class Seconds(s: int)

case class AddWatcher(actor: Player)
case class RemoveWatcher(actor: Player)
case class RemovePlayer(actor: Player)
case class AddPlayerToTable(actor: Player, seat: Int)
case class AddPlayers(players: List[Player])

case class Countdown()
case class DealCards()
case class CardsDealed(players: List[Player])
case class SmallBlind(player: Player)
case class BigBlind(player: Player)
case class SetMoney(player: Player, money: int)

case class PutSmallBlind(player: Player, money: int)
case class PutBigBlind(player: Player, money: int)

case class WaitForAction(players: List[Player], player: Player)
case class PutWait(players: List[Player], player: Player)
case class ResetGame()
case class StopCountdown()

case class Fold(player: Player)
case class Call(player: Player)
case class Check(player: Player)
case class Raise(player: Player)

case class ShowFlop(cards: List[Card])
case class ShowTurn(cards: List[Card])
case class ShowRiver(cards: List[Card])
case class ShowShowDown(players: List[Player], winner: Player)

case class ShowAllMoney(players: List[Player], money: Int)

case class SendMessage(player: String, str: String)
case class ShowMessage(player: String, str: String)
case class ShowStatusMessage(message: String)

object Table {
  lazy val table = new Table
}