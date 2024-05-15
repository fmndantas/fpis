package com.github.fmndantas.six.candyDispenser

import com.github.fmndantas.six.State
import com.github.fmndantas.six.candyDispenser.Answer
import com.github.fmndantas.six.candyDispenser.Machine

trait CandyDispenser {
  sealed trait Input
  case object Start extends Input
  case object Coin extends Input
  case object Turn extends Input

  def simulate(inputs: Seq[Input]): State[Machine, Answer] =
    (inputs :+ Start)
      .map { input =>
        input match {
          case Start => start
          case Coin  => insertCoin
          case Turn  => turnKnob
        }
      }
      .foldLeft(State.unit(Answer(-1, -1))) { (acc, s) =>
        acc.flatMap(_ => s)
      }

  private def start: State[Machine, Answer] = State {
    _.asAnsMac
  }

  private def insertCoin: State[Machine, Answer] = State {
    _.insertCoin.asAnsMac
  }

  private def turnKnob: State[Machine, Answer] = State {
    _.turnKnob.asAnsMac
  }
}
