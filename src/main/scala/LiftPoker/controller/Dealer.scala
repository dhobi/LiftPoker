package LiftPoker.controller

import java.util.Collections

import LiftPoker.comet.Player
import LiftPoker.model.{Card, Poker}
import com.sampullara.poker.{Cards, HandRank}
import net.liftweb.util.Helpers._
import net.liftweb.util.Schedule

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 24.06.2010
 * Time: 10:41:34
 * To change this template use File | Settings | File Templates.
 */

class Dealer(fromTable: Table) {
  private val poker = new Poker

  var hasStarted = false
  var isStarting = false
  private var allUsers: Map[Int, Player] = Map()
  private var waitingUsers: Map[Int, Player] = Map()
  private var position = 0
  private var dealer = 0

  private var table: Table = fromTable

  def addUser(user: Player) = {

    table.sendStatusMessage(user.playername + " has joined the table")
    if (hasStarted) {
      waitingUsers ++= List((user.seatNr - 1) -> user)
    } else {
      waitingUsers ++= List((user.seatNr - 1) -> user)
      allUsers ++= List((user.seatNr - 1) -> user)
    }



    if (allUsers.size >= 2 && !hasStarted) {
      startGame
    } else if (hasStarted) {
      table ! UpdateCards
    }
  }

  def removeUser(user: Player) = {
    val actualPlayerId = getPlayer.seatNr

    allUsers -= (user.seatNr - 1)
    waitingUsers -= (user.seatNr - 1)

    if (user.seatNr == actualPlayerId) {
      checkStep
    }

  }

  def startGame = {
    if (!isStarting && allUsers.size >= 2) {
      table.sendStatusMessage("####### New hand in 5 seconds...#######<br/>")
      table ! Countdown()
      isStarting = true
    }
  }

  def reset = {
    table.sendStatusMessage("####### End of this hand #######<br/><br/>")
    hasStarted = false;
    isStarting = false;
    position = dealer;

    allUsers = waitingUsers
    moveDealer
    startGame
  }

  def dealCards = {
    hasStarted = true
    poker.activeRound = 0
    isStarting = false;
    table ! DealCards()
    setBlinds

  }

  def setBlinds = {

    Schedule.schedule(table, SmallBlind(getPlayer), 1000L)
    movePlayer
    Schedule.schedule(table, BigBlind(getPlayer), 2000L)
    movePlayer

    Schedule.schedule(table, WaitForAction(allUsers.values.toList, getPlayer), 3000L)

  }

  def waitingForAction = {
    table ! WaitForAction(allUsers.values.toList, getPlayer)
  }

  def getPlayer = {
    allUsers.get(position).getOrElse(new Player)
  }

  def onlyOnePlayerLeft = {
    getActivePlayers.size == 1
  }

  def getActivePlayers = {
    allUsers.values.filter(p => !p.fold).toList
  }

  def findNextPosition = {
    movePlayer
    while (allUsers.get(position).getOrElse(new Player).fold) {
      movePlayer
    }
  }

  def call(player: Player) = {
    player.satisfied = true
    var amount = table.getHighestRoundMoney.get - player.usedMoney
    table ! SetMoney(player, amount)
    table.sendStatusMessage(player.playername + " calls")
    checkStep
  }

  def check(player: Player) = {
    player.satisfied = true
    table.sendStatusMessage(player.playername + " checks")
    checkStep
  }

  def raise(player: Player) = {
    var amount = table.getRaiseAmount

    getActivePlayers.foreach(p => p.satisfied = false)
    player.satisfied = true

    var plus = table.getHighestRoundMoney.get + amount - player.usedMoney

    table ! SetMoney(player, plus)

    if (table.getHighestRoundMoney.get == 0) {
      table.sendStatusMessage(player.playername + " bets " + plus)
    } else {
      table.sendStatusMessage(player.playername + " raises to " + (table.getHighestRoundMoney.get + amount))
    }


    checkStep
  }

  def fold(player: Player) = {
    //remove in active list
    player.satisfied = true
    player.fold = true
    table.sendStatusMessage(player.playername + " folds")
    checkStep

  }

  def checkStep = {
    if (onlyOnePlayerLeft) {
      getActivePlayers.head.money.add(table.allPlayedMoney)
      table ! UpdatePlayers

      Schedule.schedule(table, ResetGame(), 5000L)
    } else {
      if (getActivePlayers.filter(p => !p.satisfied).isEmpty) {

        getActivePlayers.foreach(p => p.satisfied = false)
        findNextPosition

        poker.next
        table ! poker.getActiveRound


      } else {
        findNextPosition
        waitingForAction
      }

    }

  }

  def showdown: List[Player] = {
    getActivePlayers.foreach(user => {

      var cards = new Cards(5)

      user.getCards.foreach((c: Card) => cards.add(c.getJavaCard))
      table.flop.foreach((c: Card) => cards.add(c.getJavaCard))
      table.turn.foreach((c: Card) => cards.add(c.getJavaCard))
      table.river.foreach((c: Card) => cards.add(c.getJavaCard))

      //If just someone told me that the cards has to be sorted and reversed to be evaluated correctly
      Collections.sort(cards)
      Collections.reverse(cards)

      user.setHandRank(new HandRank(cards))
    })
    getActivePlayers.sortWith((p1, p2) => p1.handrank.compareTo(p2.handrank) < 0).reverse
  }

  def updateMoney(player: Player, money: Int) = {
    player.money.remove(money)
    player.usedMoney = player.usedMoney + money
  }

  def moveDealer: Unit = {
    dealer = (dealer + 1) % (allUsers.values.toList.sortWith(_.seatNr > _.seatNr).headOption match {
      case Some(player) => player.seatNr + 1
      case None => 1
    })
    if (!allUsers.isDefinedAt(dealer) && !allUsers.isEmpty) {
      moveDealer
    }
  }

  def movePlayer: Unit = {
    position = (position + 1) % (allUsers.values.toList.sortWith(_.seatNr > _.seatNr).headOption match {
      case Some(player) => player.seatNr + 1
      case None => 1
    })
    if (!allUsers.isDefinedAt(position) && !allUsers.isEmpty) {
      movePlayer
    }
  }

}