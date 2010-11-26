package LiftPoker.snippet

import net.liftweb.util.Helpers._
import net.liftweb.http.{S, SHtml, SessionVar}
import LiftPoker.comet.Player
import net.liftweb.http.js.jquery.JqJsCmds.JqSetHtml
import xml.{Text, NodeSeq}
import LiftPoker.controller.{Table, PokerHouse}
import net.liftweb.http.js.{JsCmds, JsCmd}

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 23.06.2010
 * Time: 17:19:45
 * To change this template use File | Settings | File Templates.
 */
object currentUser extends SessionVar[String]("")
object currentTable extends SessionVar[Table](null)

class Signup {

  val headerhtml : NodeSeq = (<div style="width:400px;text-align:left;margin-left:200px;clear:left;">
    <div style="width:150px;float:left;">Tablename</div>
    <div style="width:50px;margin-left:20px;float:left;">Blinds</div>
    <div style="width:50px;margin-left:20px;float:left;">Seats</div>
  </div><br/>)
  
  val listhtml : NodeSeq = (<div style="width:400px;text-align:left;margin-left:200px;clear:left;">
    <div style="width:150px;float:left;"><t:name/></div>
    <div style="width:50px;margin-left:20px;float:left;"><t:blind/></div>
    <div style="width:50px;margin-left:20px;float:left;"><t:size/></div>
  </div>)

  def render(html:NodeSeq) : NodeSeq = {

    var user = currentUser.is

    bind("s",SHtml.ajaxForm(html),
      "name" -%> SHtml.text(user, user = _),
      "submit" -%> SHtml.ajaxSubmit("Enter", () => {currentUser(user); renderTableList}))
  }

  def renderTableList : JsCmd = {
     JqSetHtml("login_form",renderList)
  }

  def renderList = {

    headerhtml ++ PokerHouse.getTables.flatMap(table => bind("t",listhtml,
       "name" -> SHtml.a(() => {currentTable(table); JsCmds.RedirectTo("signup");}, Text(table.getName)),
      "blind" -> Text(table.getSmallBlind+"/"+table.getBigBlind), 
      "size" -> Text(table.getPlayers.size+"/"+table.getSize)))
  }

}