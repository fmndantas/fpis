package com.github.fmndantas

import com.github.fmndantas.six.State

trait CandyDispenser {
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int):
    def noCandyLeft: Boolean = candies == 0
    def createAnswer: Answer = Answer(coins, candies)
    def answerAndMachine: (Answer, Machine) = (createAnswer, this)

  case class Answer(coins: Int, candies: Int)

  def insertCoin(amount: Int): State[Machine, Answer] = State { machine =>
    machine
      .copy(locked = machine.noCandyLeft, coins = machine.coins + 1)
      .answerAndMachine
  }

  def turnKnob: State[Machine, Answer] = State { machine =>
    if (machine.locked) machine.answerAndMachine
    else
      machine
        .copy(locked = true, candies = machine.candies - 1)
        .answerAndMachine
  }
}
