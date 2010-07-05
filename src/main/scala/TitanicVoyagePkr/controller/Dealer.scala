package TitanicVoyagePkr.controller

import TitanicVoyagePkr.comet.Player
import net.liftweb.actor.LiftActor
import collection.mutable.Buffer
import collection.jcl.Buffer
import net.liftweb.util.ActorPing
import net.liftweb.util.Helpers._
import TitanicVoyagePkr.model.{Card, Poker}
import com.sampullara.poker.{HandRank, Cards}

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 24.06.2010
 * Time: 10:41:34
 * To change this template use File | Settings | File Templates.
 */

class Dealer {
  private val poker = new Poker

  var hasStarted = false
  var isStarting = false
  private var allUsers: Map[Int, Player] = Map()
  private var waitingUsers: Map[Int, Player] = Map()
  private var position = 0
  private var dealer = 0

  def addUser(user: Player) = {
    if (hasStarted) {
      waitingUsers ++= List((user.id - 1) -> user)
    } else {
      waitingUsers ++= List((user.id - 1) -> user)
      allUsers ++= List((user.id - 1) -> user)
    }



    if (allUsers.size >= 2 && !hasStarted) {
      startGame
    } else if (hasStarted) {
      Table.table.updateCards
    }
  }

  def removeUser(user: Player) = {
    allUsers -= (user.id - 1)
    waitingUsers -= (user.id - 1)
  }

  def startGame = {
    if (!isStarting && allUsers.size >= 2) {
      Table.table ! Countdown()
      isStarting = true
    }
  }

  def reset = {
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
    Table.table ! DealCards()
    setBlinds

  }

  def setBlinds = {

    ActorPing.schedule(Table.table, SmallBlind(getPlayer), 1000L)
    movePlayer
    ActorPing.schedule(Table.table, BigBlind(getPlayer), 2000L)
    movePlayer

    ActorPing.schedule(Table.table, WaitForAction(allUsers.values.toList, getPlayer), 3000L)

  }

  def waitingForAction = {
    Table.table ! WaitForAction(allUsers.values.toList, getPlayer)
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
    var amount = Table.table.getHighestRoundMoney.get - player.usedMoney
    Table.table ! SetMoney(player, amount)
    checkStep
  }

  def check(player: Player) = {
    player.satisfied = true
    checkStep
  }

  def raise(player: Player) = {
    var amount = 4

    getActivePlayers.foreach(p => p.satisfied = false)
    player.satisfied = true

    var plus = Table.table.getHighestRoundMoney.get + amount - player.usedMoney

    Table.table ! SetMoney(player, plus)
    checkStep
  }

  def fold(player: Player) = {
    //remove in active list
    player.satisfied = true
    player.fold = true
    checkStep

  }

  def timeout(player: Player) = {
    //Player is gone anyway...
    checkStep
  }

  def checkStep = {
    if (onlyOnePlayerLeft) {
      getActivePlayers.first.money.add(Table.table.allPlayedMoney)
      Table.table.updatePlayers

      ActorPing.schedule(Table.table, ResetGame(), 5000L)
    } else {
      if (getActivePlayers.filter(p => !p.satisfied).toList.size == 0) {

        getActivePlayers.foreach(p => p.satisfied = false)
        findNextPosition

        poker.next
        Table.table ! poker.getActiveRound


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
      Table.table.flop.foreach((c: Card) => cards.add(c.getJavaCard))
      Table.table.turn.foreach((c: Card) => cards.add(c.getJavaCard))
      Table.table.river.foreach((c: Card) => cards.add(c.getJavaCard))

      user.setHandRank(new HandRank(cards))


    })
    getActivePlayers.sort((p1, p2) => p1.handrank.compareTo(p2.handrank) < 0).last

  }

  def updateMoney(player: Player, money: Int) = {
    player.money.remove(money)
    player.usedMoney = player.usedMoney + money
  }

  def moveDealer : Unit = {
    dealer = (dealer + 1) % (allUsers.values.toList.sort(_.id > _.id).first.id + 1)
    if(!allUsers.isDefinedAt(dealer)) {
      moveDealer
    }
  }

  def movePlayer : Unit = {
    position = (position + 1) % (allUsers.values.toList.sort(_.id > _.id).first.id +1)
    if(!allUsers.isDefinedAt(position)) {
      movePlayer
    }
  }

}