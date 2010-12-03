package LiftPoker.snippet

import xml.NodeSeq
import net.liftweb.http.js.JsCmds
import LiftPoker.controller.{SendMessage, Table}
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JE.JsRaw

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 02.07.2010
 * Time: 10:13:12
 * To change this template use File | Settings | File Templates.
 */

class Chat {

  def render(xhtml: NodeSeq) : NodeSeq = {

    var message = ""

    bind("chat",SHtml.ajaxForm(xhtml),
        "message" -%> SHtml.text("",message = _),
        "submit" -%> SHtml.ajaxSubmit("Send", () => {currentTable.is ! SendMessage(currentUser.is,message); JsRaw("document.getElementById('message').value=''").cmd; }))
  }

}