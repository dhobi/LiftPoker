package LiftPoker.comet

import LiftPoker.controller._
import xml.NodeSeq
import net.liftweb.http.js.JsCmds
import LiftPoker.model.{Money, Card}
import com.sampullara.poker.HandRank
import net.liftweb.util.ActorPing

/**
 * Created by IntelliJ IDEA.
 * User: Dani
 * Date: 28.11.2010
 * Time: 11:31:18
 * To change this template use File | Settings | File Templates.
 */

class Bot(tabler : Table, seat : Int, name : String) extends Player {

  id = seat
  playername = name
  table = tabler

  cards = List()
  fold = false
  satisfied = false
  handrank  = null
  usedMoney = 0

  //add on instantiation
  table ! AddWatcher(this)
  table ! AddPlayerToTable(this, seat);

  override def render() = { NodeSeq.Empty}

  override def localSetup {}

  override def localShutdown {}

  override def lowPriority: PartialFunction[Any, Unit] = {
    case AddPlayers(users: List[Player]) => {}
    case Seconds(s: Int) => {}
    case StopCountdown() => {}
    case CardsDealed(users: List[Player]) => {}
    case PutSmallBlind(user: Player, money: Int) => {}
    case PutBigBlind(user: Player, money: Int) => {}
    case PutWait(users: List[Player], user: Player) => {
      updateWait(users, user)
    }
    case SetMoney(player: Player, money: Int) => {}
    case Fold(player: Player) => {}
    case ShowFlop(cards: List[Card]) => {}
    case ShowTurn(cards: List[Card]) => {}
    case ShowRiver(cards: List[Card]) => {}
    case ShowShowDown(players: List[Player], winner: Player) => {}
    case ResetGame() => {
      resetGame
    }
    case ShowAllMoney(players: List[Player], money: Int) => {}
    case ShowMessage(player: String, str: String) => {}
    case ShowStatusMessage(message: String) => {}
  }

  override def updateWait(users: List[Player], user: Player) = {
    setAction(user)
    JsCmds.Noop
  }

  override def setAction(player: Player) = {

    if (player.id == this.id) {

      val chosen = (math.random < 0.1) match {
        case true => {
          Fold(this);
        }
        case false => {
          (math.random < 0.7) match {
            case true => {
              if (usedMoney == table.getHighestRoundMoney.get) {
                Check(this)
              } else if (usedMoney < table.getHighestRoundMoney.get) {
                Call(this)
              } else {
                Check(this)
              }
            }
            case false => {
              Raise(this)
            }
          }
        }
      }
      ActorPing.schedule(table, chosen, 1500L)
    }
    JsCmds.Noop
  }

  override def resetGame() = {
    fold = false
    cards = List()
    handrank = null
    usedMoney = 0
    satisfied = false
    if (future != null) {
      future.cancel(true);
    }
    JsCmds.Noop
  }
}