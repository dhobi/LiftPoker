package LiftPoker.model

/**
 * Created by IntelliJ IDEA.
 * User: dhobi
 * Date: 28.06.2010
 * Time: 13:45:43
 * To change this template use File | Settings | File Templates.
 */

class Money(amount: Int) {
  private var value = amount

  def remove(v: Int) = {
    value = value - v
  }

  def add(v: Int) = {
    value = value + v
  }

  def get = {
    value
  }

  def hasMin(v: Int): Boolean = {
    value >= v
  }

  def +(money: Money): Money = {
    new Money(value + money.get)
  }

}