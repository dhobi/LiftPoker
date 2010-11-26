package LiftPoker.controller

import LiftPoker.comet.Player
import net.liftweb.actor.LiftActor

import net.liftweb.util.ActorPing
import net.liftweb.util.Helpers._
import LiftPoker.model.{Card, Poker}
import com.sampullara.poker.{HandRank, Cards}
import scala.collection.JavaConversions._
import java.util.Collections

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 24.06.2010
 * Time: 10:41:34
 * To change this template use File | Settings | File Templates.
 */

class Dealer(fromTable : Table) {
  private val poker = new Poker

  var hasStarted = false
  var isStarting = false
  private var allUsers: Map[Int, Player] = Map()
  private var waitingUsers: Map[Int, Player] = Map()
  private var position = 0
  private var dealer = 0

  private var table : Table = fromTable

  def addUser(user: Player) = {

    table.sendMessage(user.playername,"has joined the table")
    if (hasStarted) {
      waitingUsers ++= List((user.id - 1) -> user)
    } else {
      waitingUsers ++= List((user.id - 1) -> user)
      allUsers ++= List((user.id - 1) -> user)
    }



    if (allUsers.size >= 2 && !hasStarted) {
      startGame
    } else if (hasStarted) {
      table.updateCards
    }
  }

  def removeUser(user: Player) = {
    allUsers -= (user.id - 1)
    waitingUsers -= (user.id - 1)

    table.sendMessage(user.playername,"has left the table")

    if(user.id == getPlayer.id) {
      checkStep
    }

    if(onlyOnePlayerLeft) {
      getActivePlayers.first.money.add(table.allPlayedMoney)
      table.updatePlayers
      table ! ResetGame()
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

    ActorPing.schedule(table, SmallBlind(getPlayer), 1000L)
    movePlayer
    ActorPing.schedule(table, BigBlind(getPlayer), 2000L)
    movePlayer

    ActorPing.schedule(table, WaitForAction(allUsers.values.toList, getPlayer), 3000L)

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
    table.sendStatusMessage(player.playername+" calls")
    checkStep
  }

  def check(player: Player) = {
    player.satisfied = true
    table.sendStatusMessage(player.playername+" checks")
    checkStep
  }

  def raise(player: Player) = {
    var amount = table.getRaiseAmount

    getActivePlayers.foreach(p => p.satisfied = false)
    player.satisfied = true

    var plus = table.getHighestRoundMoney.get + amount - player.usedMoney

    table ! SetMoney(player, plus)

    if (table.getHighestRoundMoney.get == 0) {
      table.sendStatusMessage(player.playername+" bets "+plus)
    } else {
      table.sendStatusMessage(player.playername+" raises to "+(table.getHighestRoundMoney.get + amount))
    }


    checkStep
  }

  def fold(player: Player) = {
    //remove in active list
    player.satisfied = true
    player.fold = true
    table.sendStatusMessage(player.playername+" folds")
    checkStep

  }

  def checkStep = {
    if (onlyOnePlayerLeft) {
      getActivePlayers.first.money.add(table.allPlayedMoney)
      table.updatePlayers

      ActorPing.schedule(table, ResetGame(), 5000L)
    } else {
      if (getActivePlayers.filter(p => !p.satisfied).toList.size == 0) {

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

  def showdown: Player = {
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
    var winner = getActivePlayers.sort((p1, p2) => p1.handrank.compareTo(p2.handrank) < 0).last
    table.sendStatusMessage(winner.playername+" wins with "+winner.handrank)
    winner

  }

  def updateMoney(player: Player, money: Int) = {
    player.money.remove(money)
    player.usedMoney = player.usedMoney + money
  }

  def moveDealer: Unit = {
    dealer = (dealer + 1) % (allUsers.values.toList.sort(_.id > _.id).first.id + 1)
    if (!allUsers.isDefinedAt(dealer)) {
      moveDealer
    }
  }

  def movePlayer: Unit = {
    position = (position + 1) % (allUsers.values.toList.sort(_.id > _.id).first.id + 1)
    if (!allUsers.isDefinedAt(position)) {
      movePlayer
    }
  }

}