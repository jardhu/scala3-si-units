package si

private[si] object Ints {
  /** Type representation of all positive and negative integers. This is implemented via
    * type statements representing the Peano axioms along with a negation type, i.e.
    * the natural numbers plus the operation of negation.
    */
  trait TypedInteger {
    def ordinal: Int
  }
  /** The positive natural numbers, strictly excluding zero.
    */
  trait PositiveInt extends TypedInteger

  /** The first number defining the natural numbers.
    */
  case class Zero() extends TypedInteger {
    def ordinal: Int = 0
  }
  /** Inductive case defining all other natural numbers. Also defines all strictly
    * positive, non-zero numbers.
    */
  case class SuccessorOf[N <: Zero | PositiveInt](n: N) extends PositiveInt {
    def ordinal: Int = 1 + n.ordinal
  }
  /** The negatives of the positive natural numbers, strictly excluding zero.
    */
  case class NegativeInt[N <: PositiveInt](n: N) extends TypedInteger {
    def ordinal: Int = -n.ordinal
  }

  /** Negation operation on all non-zero integers. */
  type NegativeOf[NP <: TypedInteger] <: TypedInteger = NP match {
    case Zero => Zero
    case NegativeInt[n] => n
    case _ => NegativeInt[NP]
  }

  /** Type representations of the addition and subtraction operations over the integral types. 
    */
  type Add[A <: TypedInteger, B <: TypedInteger] <: TypedInteger = (A, B) match {
    // Trivial cases
    case (_, Zero) => A
    case (Zero, _) => B

    // If both numbers are natural numbers
    case (SuccessorOf[prevA], SuccessorOf[prevB]) => SuccessorOf[SuccessorOf[Add[prevA, prevB]]]

    // Negative numbers: Attempt to reduce operations down to additions or subtractions of natural numbers.

    // If both nuInters are negative:   -A + -B = -(A + B)
    case (NegativeInt[nA], NegativeInt[nB]) => NegativeInt[Add[nA, nB]]
    // If the right number is negative: A + -B => A - B
    case (_, NegativeInt[nB]) => Sub[A, nB]
    // If the left number is negative: -A + B = B - A
    case (NegativeInt[nA], _) => Sub[nA, B]
  }

  type Sub[A <: TypedInteger, B <: TypedInteger] <: TypedInteger = (A, B) match {
    // Trivial cases
    case (_, Zero) => A
    case (Zero, _) => NegativeInt[B]
    
    // If both numbers are natural numbers
    case (SuccessorOf[prevA], SuccessorOf[prevB]) => SuccessorOf[SuccessorOf[Sub[prevA, prevB]]]

    // Negative numbers: Attempt to reduce operations down to additions or subtractions of natural numbers.
    
    // If both numbers are negative:   -A - -B => B - A
    case (NegativeInt[nA], NegativeInt[nB]) => Sub[nB, nA]
    // If the right number is negative: A - (-B) => A + B
    case (_, NegativeInt[nB]) => Add[A, nB]
    // If the left number is negative: -A - B => -(A + B)
    case (NegativeInt[nA], _) => NegativeInt[Add[nA, B]]
  }


  /** Convenience types
    */
  type One = SuccessorOf[Zero]

  /** Given statements which construct case class instances of the integral types.
    * Used by SI#show to create concrete, printable values for each integral type.
    *
    * Adapted from: https://github.com/Bersier/physical
    */
  given zero: Ints.Zero = Ints.Zero()
  given [N <: Ints.Zero | Ints.PositiveInt](using n: N): Ints.SuccessorOf[N] = Ints.SuccessorOf(n)
  given [N <: Ints.PositiveInt](using n: N): Ints.NegativeInt[N] = Ints.NegativeInt(n)
}
