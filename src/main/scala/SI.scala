package si

import si.Ints.*
import scala.math.abs

object SIUnits {
  /* Generic SI type.
   */
  opaque type SI = Double

  /** Concrete type implementation for all SI units; opaquely implemented as a double.
    * Type representation is a 7-dimensional vector where each dimension is a integral
    * type for the exponent of a base unit (kg, m, s, ...). This allows the SI units to
    * be expressed as operations of linear algebra via vector addition, subtraction, etc.
    */

  opaque type SIVec[
    KG  <: TypedInteger,
    M   <: TypedInteger,
    S   <: TypedInteger,
    A   <: TypedInteger,
    K   <: TypedInteger,
    MOL <: TypedInteger,
    CD  <: TypedInteger
  ] <: SI = Double
  
  /** DEFINE BASE SI UNITS **/

  /** The dimensionless unit represented by <1>. */
  type one = SIVec[Zero, Zero, Zero, Zero, Zero, Zero, Zero]
   
  /** The kilogram, base SI unit of mass. */
  type kg  = SIVec[One, Zero, Zero, Zero, Zero, Zero, Zero]

  /** The metre, base SI unit of length. */
  type m   = SIVec[Zero, One, Zero, Zero, Zero, Zero, Zero]

  /** The second, base SI unit of time. */
  type s   = SIVec[Zero, Zero, One, Zero, Zero, Zero, Zero]

  /** The ampere, base SI unit of electric current. */
  type A   = SIVec[Zero, Zero, Zero, One, Zero, Zero, Zero]

  /** The kelvin, base SI unit of temperature. */
  type K   = SIVec[Zero, Zero, Zero, Zero, One, Zero, Zero]

  /** The mole, base SI unit of quantity. */
  type mol = SIVec[Zero, Zero, Zero, Zero, Zero, One, Zero]

  /** The candela, base SI unit of luminous intensity. */
  type cd  = SIVec[Zero, Zero, Zero, Zero, Zero, Zero, One]



  private[si] object SI {
    /** Converts a double to some arbitrary SI unit. */
    def apply[U <: SI](d: Double): U = d.asInstanceOf[U]
  }

  /** For a given SI unit U, derives the unit U^-1, or 1 / U. */
  type Invert[U <: SI] <: SI = U match {
    case SIVec[uKG, uM, uS, uA, uK, uMOL, uCD] =>
      SIVec[NegativeOf[uKG], NegativeOf[uM], NegativeOf[uS], NegativeOf[uA], NegativeOf[uK], NegativeOf[uMOL], NegativeOf[uCD]]
  }

    /** Derives an SI unit via the product of two constituent SI units. */
  type *[A <: SI, B <: SI] <: SI = (A, B) match {
    case (SIVec[aKG, aM, aS, aA, aK, aMOL, aCD], SIVec[bKG, bM, bS, bA, bK, bMOL, bCD]) =>
      SIVec[Add[aKG, bKG], Add[aM, bM], Add[aS, bS], Add[aA, bA], Add[aK, bK], Add[aMOL, bMOL], Add[aCD, bCD]]
  }

  /** Derives an SI unit via the quotient of two constituent SI units. */
  type /[A <: SI, B <: SI] = A * Invert[B]

  /** Define implicit conversions from Scala base numerics to the dimensionless unit */
  given [T : Numeric]: Conversion[T, one] = (x: T) => SI[one](implicitly[Numeric[T]].toDouble(x))

  /** Define implicit conversions from dimensionless units to Scala base numerics */  
  //given [T : Numeric]: Conversion[one, T] = (x: T) => implicitly[Numeric[T]].fromDouble(x)
  given Conversion[one, BigInt] = (x: one) => BigInt(x)
  given Conversion[one, Int] = (x: one) => x.toInt
  given Conversion[one, Short] = (x: one) => x.toShort
  given Conversion[one, Byte] = (x: one) => x.toByte
  given Conversion[one, Char] = (x: one) => x.toChar
  given Conversion[one, Long] = (x: one) => x.toLong
  given Conversion[one, Float] = (x: one) => x.toFloat
  given Conversion[one, Double] = (x: one) => x
  given Conversion[one, BigDecimal] = (x: one) => BigDecimal(x)

  /** Define multiversal equality between same-typed SI units. */ 
  given [ 
    uKG <: TypedInteger,
    uM <: TypedInteger,
    uS <: TypedInteger,
    uA <: TypedInteger,
    uK <: TypedInteger,
    uMOL <: TypedInteger,
    uCD <: TypedInteger,
    U <: SIVec[uKG, uM, uS, uA, uK, uMOL, uCD]
  ]: CanEqual[U, U] = CanEqual.derived

  /** Define compile-time operations over different SI units (multiplication, division) */
  extension[U <: SI, V <: SI] (x: U) {
    def * (y: V): U * V = SI[U * V](x * y)
    def / (y: V): U / V = SI[U / V](x / y)
  }

  /** Define compile-time operations over same-typed SI units (addition, subtraction, comparisons, #show) */
  extension[
    uKG <: TypedInteger,
    uM <: TypedInteger,
    uS <: TypedInteger,
    uA <: TypedInteger,
    uK <: TypedInteger,
    uMOL <: TypedInteger,
    uCD <: TypedInteger,
    U <: SIVec[uKG, uM, uS, uA, uK, uMOL, uCD]
  ] (x: U) {
    def + (y: U): U = SI[U](x + y)
    def - (y: U): U = SI[U](x - y)

    def < (y: U): Boolean  = x < y
    def > (y: U): Boolean  = x > y
    def <= (y: U): Boolean = x <= y
    def >= (y: U): Boolean = x >= y

    /** Prints out the quantity of the provided SI unit value along with its dimensional counterpart.
      *
      * Summons case class instances of each integral type for use in printing out each unit and exponent.
      * Adapted from: https://github.com/Bersier/physical
      */
    def show(using uKG, uM, uS, uA, uK, uMOL, uCD): String = s"${x.toDouble} ${unitSuffix(
      summon[uKG],
      summon[uM],
      summon[uS],
      summon[uA],
      summon[uK],
      summon[uMOL],
      summon[uCD]
    )}"
  }

  /** Define the explicit .as[U] operation to convert numerical values to a specific SI unit U */
  extension[T : Numeric] (x: T) {
    def as[U <: SI]: U = SI[U](implicitly[Numeric[T]].toDouble(x))
  }

  /** Helper function to print the dimensional part of an SI value.
    */
  private def unitSuffix[
    KG <: TypedInteger,
    M <: TypedInteger,
    S <: TypedInteger,
    A <: TypedInteger,
    K <: TypedInteger,
    MOL <: TypedInteger,
    CD <: TypedInteger
  ](kilograms: KG, metres: M, seconds: S, amperes: A, kelvin: K, moles: MOL, candelas: CD): String = {
    val units = Seq(kilograms, metres, seconds, amperes, kelvin, moles, candelas)
      .map(_.ordinal)
      .zip(Seq("kg", "m", "s", "A", "K", "mol", "cd"))

    val negativeUnits = units.filter((exp, _) => exp < 0).map(unitStr)
    val positiveUnits = units.filter((exp, _) => exp > 0).map(unitStr)

    val numeratorStr = (positiveUnits, negativeUnits) match {
      case (Nil, Nil)     => "<1>"
      case (Nil, Seq(_*)) => "1"
      case (Seq(_*), _)   => positiveUnits.mkString("‧")
    }

    val denominatorStr = negativeUnits match {
      case Nil            => "" 
      case Seq(_*)        => s"/${negativeUnits.mkString("‧")}"
    }

    s"${numeratorStr}${denominatorStr}"
  }

  private def unitStr(exp: Int, unit: String): String = s"$unit${if (abs(exp) > 1) s"^${abs(exp)}" else ""}"
}
