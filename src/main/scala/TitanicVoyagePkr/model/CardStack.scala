package TitanicVoyagePkr.model

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 23.06.2010
 * Time: 18:40:39
 * To change this template use File | Settings | File Templates.
 */

class CardStack {
  var cards: List[Card] = shuffle((new Card("h",2) :: new Card("h",3) ::
          new Card("h",4) :: new Card("h",5) ::
          new Card("h",6) :: new Card("h",7) ::
          new Card("h",8) :: new Card("h",9) ::
          new Card("h",10) :: new Card("h",11) ::
          new Card("h",12) :: new Card("h",13) ::
          new Card("ka",2) :: new Card("ka",3) ::
          new Card("ka",4) :: new Card("ka",5) ::
          new Card("ka",6) :: new Card("ka",7) ::
          new Card("ka",8) :: new Card("ka",9) ::
          new Card("ka",10) :: new Card("ka",11) ::
          new Card("ka",12) :: new Card("ka",13) ::
          new Card("kr",2) :: new Card("kr",3) ::
          new Card("kr",4) :: new Card("kr",5) ::
          new Card("kr",6) :: new Card("kr",7) ::
          new Card("kr",8) :: new Card("kr",9) ::
          new Card("kr",10) :: new Card("kr",11) ::
          new Card("kr",12) :: new Card("kr",13) ::
          new Card("p",2) :: new Card("p",3) ::
          new Card("p",4) :: new Card("p",5) ::
          new Card("p",6) :: new Card("p",7) ::
          new Card("p",8) :: new Card("p",9) ::
          new Card("p",10) :: new Card("p",11) ::
          new Card("p",12) :: new Card("p",13) :: Nil).toArray[Card]).toList



  def shuffle[T](array: Array[T]): Array[T] = {
    val rnd = new java.util.Random
    for (n <- Iterator.range(array.length - 1, 0, -1)) {
      val k = rnd.nextInt(n + 1)
      val t = array(k); array(k) = array(n); array(n) = t
    }
    return array
  }


  def getCards(i:int) = {
    var playercards = cards.take(i)
    cards = cards.remove(playercards.contains(_))
    playercards
  }

}