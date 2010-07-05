package TitanicVoyagePkr.model

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 28.06.2010
 * Time: 13:45:43
 * To change this template use File | Settings | File Templates.
 */

class Money(amount : int) {
  private var value = amount

  def remove(v : int) = {
    value = value - v
  }

  def add(v : int) = {
    value = value + v
  }

  def get = {
    value
  }

  def hasMin(v:int) : Boolean = {
    value >= v
  }

  def +(money: Money) : Money = {
     new Money(value+money.get)
  }

}