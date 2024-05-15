package com.github.fmndantas

import com.github.fmndantas.six.State

trait CandyDispenser {
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Answer(coins: Int, candies: Int)

  case class Machine(locked: Boolean, candies: Int, coins: Int):
    def noCandyLeft: Boolean = candies == 0
    def unlocked = !locked
    def lock = this.copy(locked = true)
    def insertCoin = this.copy(coins = this.coins + 1)
    def unlock = this.copy(locked = false)
    def dispenseCandy = this.copy(candies = this.candies - 1)
    def answerAndMachine: (Answer, Machine) = (Answer(coins, candies), this)

  def insertCoin: State[Machine, Answer] = State { machine =>
    if (machine.unlocked || machine.noCandyLeft) machine.answerAndMachine
    else machine.unlock.insertCoin.answerAndMachine
  }

  def turnKnob: State[Machine, Answer] = State { machine =>
    if (machine.locked || machine.noCandyLeft) machine.answerAndMachine
    else machine.dispenseCandy.lock.answerAndMachine
  }
}
