import com.github.fmndantas.six.RNG

object Utils:
  def fixedRand[A](desiredValue: A)(rng: RNG) = (desiredValue, rng)

  def fixedRNG(a: Int): RNG =
    new RNG:
      def nextInt = (a, fixedRNG(a))

  def stairRNG(initial: Int): RNG =
    new RNG:
      def nextInt = (initial, stairRNG(initial + 1))
      
