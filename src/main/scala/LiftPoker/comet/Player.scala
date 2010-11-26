package LiftPoker.comet

import net.liftweb.http.js.jquery.JqJsCmds.JqSetHtml
import net.liftweb.http.{SHtml, CometActor}
import net.liftweb.http.js.{JE, JsCmds, JsCommands, JsCmd}
import LiftPoker.controller._
import net.liftweb.util.ActorPing
import com.sampullara.poker.HandRank
import net.liftweb.http.js.JsCmds.{Alert, SetValById}
import LiftPoker.model.{Money, Card}
import xml.{Node, Elem, NodeSeq, Text}
import java.util.concurrent.ScheduledFuture
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JE.{Str, JsVal, JsRaw}
import net.liftweb.http.js.JsCommands._
import LiftPoker.snippet.{currentTable, currentUser}

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 23.06.2010
 * Time: 11:30:47
 * To change this template use File | Settings | File Templates.
 */

class Player extends CometActor {
  var id: Int = 0
  var playername: String = currentUser.is
  var table: Table = currentTable.is

  var cards: List[Card] = List()
  var fold = false
  var satisfied = false
  var handrank: HandRank = _
  val money = new Money(500)
  var usedMoney = 0



  var future: ScheduledFuture[Unit] = null

  def addCards(cards: List[Card]) = {
    if (!hasCards) this.cards = cards
  }

  def hasCards = {
    cards.size == 2
  }

  def getCards = {
    cards
  }

  def setHandRank(rank: HandRank) = {
    handrank = rank
  }

  override def render() = {

    table = currentTable.is
    playername = currentUser.is

    table.updatePlayers
    table.updateCards

    table ! AddWatcher(this)

    bind("comet", table.getXML,
      "player1name" -> getPlayerName(1),
      "player2name" -> getPlayerName(2),
      "player3name" -> getPlayerName(3),
      "player4name" -> getPlayerName(4),
      "player5name" -> getPlayerName(5),
      "player6name" -> getPlayerName(6),
      "player7name" -> getPlayerName(7),
      "player8name" -> getPlayerName(8)

      )
  }

  def getPlayerName(i: Int): NodeSeq = {
    if (table.getPlayers.isDefinedAt(i)) {
      Text(table.getPlayers.get(i).get.playername)
    } else {
      SHtml.a(() => {table ! AddPlayerToTable(this, i); clearNames; }, Text("Sit down"))
    }
  }

  override def localSetup {
    super.localSetup
  }

  override def localShutdown {
    table ! RemoveWatcher(this)
    table ! RemovePlayer(this)
    super.localShutdown
  }

  override def lowPriority: PartialFunction[Any, Unit] = {
    case AddPlayers(users: List[Player]) => {
      partialUpdate(updatePlayers(users))
    }
    case Seconds(s: Int) => {
      partialUpdate(setCountdown(s))
    }
    case StopCountdown() => {
      partialUpdate(stopCountdown)
    }
    case CardsDealed(users: List[Player]) => {
      partialUpdate(updateCards(users))
    }
    case PutSmallBlind(user: Player, money: Int) => {
      partialUpdate(updateMoney(user, money))
    }
    case PutBigBlind(user: Player, money: Int) => {
      partialUpdate(updateMoney(user, money))
    }
    case PutWait(users: List[Player], user: Player) => {
      partialUpdate(updateWait(users, user))
    }
    case SetMoney(player: Player, money: Int) => {
      partialUpdate(updateMoney(player, money))
    }
    case Fold(player: Player) => {
      partialUpdate(foldPlayer(player))
    }
    case ShowFlop(cards: List[Card]) => {
      partialUpdate(showFlop(cards))
    }
    case ShowTurn(cards: List[Card]) => {
      partialUpdate(showTurn(cards))
    }
    case ShowRiver(cards: List[Card]) => {
      partialUpdate(showRiver(cards))
    }
    case ShowShowDown(players: List[Player], winner: Player) => {
      partialUpdate(showShowDown(players, winner))
    }
    case ResetGame() => {
      partialUpdate(resetGame)
    }
    case ShowAllMoney(players: List[Player], money: Int) => {
      partialUpdate(clearMoney(players) & showAllMoney(money))
    }
    case ShowMessage(player: String, str: String) => {
      partialUpdate(JsRaw(";document.getElementById('chatmessages').innerHTML += '" + player + ": " + str.replace("&", "&amp;").replace("'", "&#39;") + "<br/>';document.getElementById('chatmessages').scrollTop = document.getElementById('chatmessages').scrollHeight;").cmd)
    }
    case ShowStatusMessage(message: String) => {
      partialUpdate(JsRaw(";document.getElementById('status_chatmessages').innerHTML += '" + message + "<br/>';document.getElementById('status_chatmessages').scrollTop = document.getElementById('status_chatmessages').scrollHeight;").cmd)
    }
  }

  def clearNames: JsCmd = {
    (1 to table.getSize).map(i => {
      var html: NodeSeq = Text("")
      if (id == 0) {
        html = SHtml.a(() => {table ! AddPlayerToTable(this, i); clearNames; }, Text("Sit down"))
      }
      JqSetHtml("player" + i + "name", html)
    }).foldLeft[JsCmd](JsCmds.Noop)(_ & _)
  }

  def updatePlayers(users: List[Player]): JsCmd = {
    clearNames & users.map(user => JqSetHtml("player" + user.id + "name", Text(user.playername) ++ <span style="margin-left:20px;">
      {user.money.get}
    </span> ++ showExit(user))).foldLeft[JsCmd](JsCmds.Noop)(_ & _)
  }

  def showExit(user: Player) = {
    if (this.id == user.id) {
      <span style="margin-left:20px;">
        {SHtml.a(() => {table ! RemovePlayer(this); currentTable(null); resetGame; JsCmds.RedirectTo("/"); }, Text("x"))}
      </span>
    } else {
      <span style="margin-left:20px;"></span>
    }
  }


  def updateCards(users: List[Player]): JsCmd = {
    var cardid = 0;
    users.map(user => {
      cardid = 0;
      user.getCards.map(card => {
        cardid = cardid + 1
        var img = "images/deckblatt.png"
        if (user.id == this.id) {
          img = card.getImage
        }
        JqSetHtml("player" + user.id + "card" + cardid, <img src={img} alt=" "/>)
      }
        ).foldLeft[JsCmd](JsCmds.Noop)(_ & _)
    }
      ).foldLeft[JsCmd](JsCmds.Noop)(_ & _)
  }

  def updateMoney(user: Player, money: Int): JsCmd = {
    JqSetHtml("player" + user.id + "money", <img src="images/chip.png" alt=" "/> <span>
      {money}
    </span>)
  }

  def clearMoney(players: List[Player]): JsCmd = {
    players.map(p => {
      JqSetHtml("player" + p.id + "money", Text(""))
    }).foldLeft[JsCmd](JsCmds.Noop)(_ & _)
  }

  def updateWait(users: List[Player], user: Player): JsCmd = {
    updatePlayers(users) &
            clearNamePlates &
            JE.JsRaw("document.getElementById('player" + user.id + "name').setAttribute('class','playernameactive')").cmd &
            setAction(user)
  }

  def clearNamePlates: JsCmd = {
    (1 to table.getSize).map(i => JE.JsRaw("document.getElementById('player" + i + "name').setAttribute('class','playername')").cmd).foldLeft[JsCmd](JsCmds.Noop)(_ & _)
  }

  def setCountdown(s: Int) = {
    if (s == 0) {
      JsCmds.JsHideId("countdown")
    } else {
      JsCmds.JsShowId("countdown") & JqSetHtml("countdown", Text(s.toString))
    }
  }

  def stopCountdown = {
    JsRaw("countdown.stop()").cmd
  }

  def setAction(player: Player) = {
    var html: Seq[Node] = new Text("")

    if (player.id == this.id) {

      //setTimeout
      future = ActorPing.schedule(table, RemovePlayer(this), 35000L)

      html = SHtml.a(() => {table ! Fold(this); future.cancel(true); hideActionForm}, <span class="action">Fold</span>)
      if (usedMoney == table.getHighestRoundMoney.get) {
        html = html ++ SHtml.a(() => {table ! Check(this); future.cancel(true); hideActionForm}, <span class="action">Check</span>)
      } else if (usedMoney < table.getHighestRoundMoney.get) {
        html = html ++ SHtml.a(() => {table ! Call(this); future.cancel(true); hideActionForm}, <span class="action">Call</span>)
      }

      if (table.getHighestRoundMoney.get == 0) {
        html = html ++ SHtml.a(() => {table ! Raise(this); future.cancel(true); hideActionForm}, <span class="action">Bet</span>)
      } else {
        html = html ++ SHtml.a(() => {table ! Raise(this); future.cancel(true); hideActionForm}, <span class="action">Raise</span>)
      }

    }
    JqSetHtml("player" + id + "action", html) & JsRaw("countdown.stop();countdown.start('player" + player.id + "countdown')").cmd
  }

  def hideActionForm: JsCmd = {
    JqSetHtml("player" + id + "action", Text(""))

  }

  def foldPlayer(player: Player) = {
    var cardid = 0
    player.getCards.map(card => {
      cardid = cardid + 1
      JqSetHtml("player" + player.id + "card" + cardid, <img src="images/x.png" alt=" "/>)
    }).foldLeft[JsCmd](JsCmds.Noop)(_ & _)
  }

  def resetGame() = {
    fold = false
    cards = List()
    handrank = null
    usedMoney = 0
    satisfied = false
    if (future != null) {
      future.cancel(true);
    }

    JqSetHtml("player1card1", Text("")) & JqSetHtml("player1card2", Text("")) &
            JqSetHtml("player2card1", Text("")) & JqSetHtml("player2card2", Text("")) &
            JqSetHtml("player3card1", Text("")) & JqSetHtml("player3card2", Text("")) &
            JqSetHtml("player4card1", Text("")) & JqSetHtml("player4card2", Text("")) &
            JqSetHtml("player5card1", Text("")) & JqSetHtml("player5card2", Text("")) &
            JqSetHtml("player6card1", Text("")) & JqSetHtml("player6card2", Text("")) &
            JqSetHtml("player7card1", Text("")) & JqSetHtml("player7card2", Text("")) &
            JqSetHtml("player8card1", Text("")) & JqSetHtml("player8card2", Text("")) &
            JqSetHtml("player1money", Text("")) &
            JqSetHtml("player2money", Text("")) &
            JqSetHtml("player3money", Text("")) &
            JqSetHtml("player4money", Text("")) &
            JqSetHtml("player5money", Text("")) &
            JqSetHtml("player6money", Text("")) &
            JqSetHtml("player7money", Text("")) &
            JqSetHtml("player8money", Text("")) &
            JqSetHtml("player1action", Text("")) &
            JqSetHtml("player2action", Text("")) &
            JqSetHtml("player3action", Text("")) &
            JqSetHtml("player4action", Text("")) &
            JqSetHtml("player5action", Text("")) &
            JqSetHtml("player6action", Text("")) &
            JqSetHtml("player7action", Text("")) &
            JqSetHtml("player8action", Text("")) &
            JqSetHtml("flop1", Text("")) &
            JqSetHtml("flop2", Text("")) &
            JqSetHtml("flop3", Text("")) &
            JqSetHtml("turn", Text("")) &
            JqSetHtml("river", Text("")) &
            JsCmds.JsHideId("allmoney") &
            JsCmds.JsHideId("winner") &
            JsRaw("countdown.stop();").cmd &
            hideActionForm
  }

  def showFlop(cards: List[Card]): JsCmd = {
    var cardid = 0
    cards.map(card => {
      cardid = cardid + 1
      JqSetHtml("flop" + cardid, <img src={card.getImage} alt=" "/>)
    }).foldLeft[JsCmd](JsCmds.Noop)(_ & _)
  }

  def showTurn(cards: List[Card]): JsCmd = {
    JqSetHtml("turn", <img src={cards.first.getImage} alt=" "/>)
  }

  def showRiver(cards: List[Card]): JsCmd = {
    JqSetHtml("river", <img src={cards.first.getImage} alt=" "/>)
  }

  def showShowDown(users: List[Player], winner: Player): JsCmd = {
    var cardid = 0;
    users.map(user => {
      cardid = 0;
      user.getCards.map(card => {
        cardid = cardid + 1
        var img = card.getImage
        if (user.fold) {
          img = "images/x.png"
        }
        JqSetHtml("player" + user.id + "card" + cardid, <img src={img} alt=" "/>)
      }
        ).foldLeft[JsCmd](JsCmds.Noop)(_ & _)
    }
      ).foldLeft[JsCmd](JsCmds.Noop)(_ & _) &
            showWinner(winner) &
            updatePlayers(users)
  }

  def showWinner(winner: Player): JsCmd = {
    JE.JsRaw("document.getElementById('player" + winner.id + "name').setAttribute('class','playernamewinner')").cmd &
            JqSetHtml("winner", Text(winner.playername + " wins with " + winner.handrank.getRank.toString)) & JsCmds.JsShowId("winner")
  }

  def showAllMoney(money: Int): JsCmd = {
    JsCmds.JsShowId("allmoney") & JqSetHtml("allmoney", <img src="images/chip.png" alt=" "/> <span>
      {money}
    </span>)
  }

}