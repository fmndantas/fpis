import com.github.fmndantas.six.candyDispenser.CandyDispenser
import com.github.fmndantas.six.candyDispenser.Machine
import com.github.fmndantas.six.candyDispenser.Answer

class CandyDispenserSpec extends MultipleCases {
  object sut extends CandyDispenser
  import sut.{Coin, Turn}

  cases(
    "Accepts sequence of inputs and returns final state based on some initial state"
  )(
    (2, 1, Seq(), Answer(coins = 2, candies = 1)),
    (0, 1, Seq(Coin, Turn), Answer(coins = 1, candies = 0)),
    (2, 0, Seq(Coin, Turn), Answer(coins = 2, candies = 0)),
    (2, 1, Seq(Coin, Turn), Answer(coins = 3, candies = 0)),
    (2, 1, Seq(Coin, Coin), Answer(coins = 3, candies = 1)),
    (
      10,
      5,
      Seq.fill(4)(Seq(Coin, Turn)).flatten,
      Answer(coins = 14, candies = 1)
    )
  ) { case (initialCoins, initialCandies, inputs, expectedAnswer) =>
    val transitions = sut.simulate(inputs)
    // NOTE: machine is initially locked
    val (answer, _) =
      transitions.run(Machine(true, initialCandies, initialCoins))
    assertEquals(answer, expectedAnswer)
  }
}
