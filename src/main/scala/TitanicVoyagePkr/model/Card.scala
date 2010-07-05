package TitanicVoyagePkr.model

import com.sampullara.poker.Card

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 23.06.2010
 * Time: 18:38:38
 * To change this template use File | Settings | File Templates.
 */

class Card(c : String, v : int) {

  private var color : String = c
  private var value : Int = v

  private var arr = Array("1","2","3","4","5","6","7","8","9","10","bube","dame","konig","ass")

  def getImage = {
    "images/"+color+""+arr(value)+".png"
  }

  def getColor = {
    
  }

  def getValue = {
    value
  }

  def getJavaCard() : com.sampullara.poker.Card = {
    var num = Array("1","2","3","4","5","6","7","8","9","t","j","q","k","a")
    var convert = Map("h"->"h","ka"->"d","kr"->"c","p"->"s")

    new  com.sampullara.poker.Card(Card.Rank.parse(num(value)),Card.Suit.parse(convert.get(color).get))
  }
}