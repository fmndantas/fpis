package com.github.fmndantas.six.candyDispenser

case class Machine(locked: Boolean, candies: Int, coins: Int):
  def outOfCandies: Boolean = candies == 0

  def unlocked = !locked

  def insertCoin = {
    if (unlocked || outOfCandies) this.copy()
    else copy(locked = false, coins = this.coins + 1)
  }

  def turnKnob = {
    if (locked || outOfCandies) this.copy()
    else copy(locked = true, candies = this.candies - 1)
  }

  def asAnsMac: (Answer, Machine) = (Answer(coins, candies), this)
