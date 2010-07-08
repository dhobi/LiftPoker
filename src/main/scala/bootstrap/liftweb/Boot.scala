package bootstrap.liftweb

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import TitanicVoyagePkr.snippet.{currentTable, currentUser}

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("TitanicVoyagePkr")

    val ifUserSet = If(
      () => !currentUser.is.equals("") && currentTable.is != null,
      () => RedirectWithState("/", RedirectState(() => S.error("Namen angeben"))))

    val ifUserNotSet = If(
      () => currentUser.is.equals("") || currentTable.is == null,
      () => RedirectWithState("/signup", RedirectState(() => S.error(""))))

    // Build SiteMap
    val entries = Menu(Loc("Home", List("index"), "Home", ifUserNotSet)) ::
                  Menu(Loc("Signup", List("signup"), "Signup", ifUserSet)) :: Nil
    LiftRules.setSiteMap(SiteMap(entries:_*))
  }
}

