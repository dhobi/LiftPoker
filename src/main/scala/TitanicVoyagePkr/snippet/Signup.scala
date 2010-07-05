package TitanicVoyagePkr.snippet

import TitanicVoyagePkr.model.User
import xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.http.{S, SHtml, SessionVar}
import TitanicVoyagePkr.comet.Player

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 23.06.2010
 * Time: 17:19:45
 * To change this template use File | Settings | File Templates.
 */
object currentUser extends SessionVar[String]("")

class Signup {

  def render(html:NodeSeq) : NodeSeq = {

    var user = currentUser.is

    bind("s",html,
      "name" -%> SHtml.text(user, user = _),
      "submit" -%> SHtml.submit("Enter", () => {currentUser(user); S.redirectTo("signup")}))
  }

}