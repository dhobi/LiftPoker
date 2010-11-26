package bootstrap.liftweb

import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._
import LiftPoker.snippet.{currentTable, currentUser}

/**
* A class that's instantiated early and run. It allows the application
* to modify lift's environment
*/
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("LiftPoker")

    val ifUserSet = If(
      () => !currentUser.is.equals("") && currentTable.is != null,
      () => RedirectWithState("/", RedirectState(() => S.error("Please give in a name."))))

    val ifUserNotSet = If(
      () => currentUser.is.equals("") || currentTable.is == null,
      () => RedirectWithState("/signup", RedirectState(() => S.error(""))))

    // Build SiteMap
    val entries = Menu(Loc("Home", List("index"), "Home", ifUserNotSet)) ::
                  Menu(Loc("Signup", List("signup"), "Signup", ifUserSet)) :: Nil
    LiftRules.setSiteMap(SiteMap(entries:_*))
  }
}
