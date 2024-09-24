import si.SIUnits.*
import scala.language.strictEquality

@main def playground: Unit = {



  println()
  println("(a) Underlying values are Double")
  println("(k) The unit-handling mechanism dissolves at compile-time")

  val aDistance = 3.523.as[m]
  println(aDistance match {
    case _: Double => "Is a double"
    case _ => "Not a double"
  }) // => "Is a double"



  println()
  println("(b) Operators +, - between inconsistent values must be rejected at compile time")

  val someTime = 2.132.as[s]

  // Both lines compile to type mismatch errors
  // Message could be better, as the raw SIVec is exposed in the error
  // val badSum  = aDistance + someTime
  // val badDiff = aDistance - someTime

  // Check that +, - are consistent with same units

  val anotherTime = 0.868.as[s]

  val good = someTime + anotherTime
  println(good.show) // "3.0 s"



  println()
  println("(c) Operators *, / must return a value of the correct unit")

  // Simple example: calculate speed
  val someSpeed = aDistance / someTime
  println(someSpeed.show) // "1.6524390243902438 m/s"


  // Complex example: calculate a capacitance in farads
  val acceleration = someSpeed / 2.as[s]

  val force   = 6.125.as[kg] * acceleration // N = kg⋅m / s^2
  val energy  = force * 3.as[m]             // J = kg⋅m^2 / s^2
  val charge  = 2.3.as[A] * 2.as[s]         // C = A⋅s
  val voltage = energy / charge             // V = kg⋅m^2 / s^3⋅A
  val capacitance = 4.2.as[A*s] / voltage   // F = s^4⋅A^2 / kg⋅m^2

  println(s"Acceleration: ${acceleration.show}") // "0.8262195121951219 m/s^2"
  println(s"Force:        ${force.show}")        // "5.060594512195122 kg‧m/s^2"
  println(s"Energy:       ${energy.show}")       // "15.181783536585366 kg‧m^2/s^2"
  println(s"Charge:       ${charge.show}")       // "4.6 s‧A"
  println(s"Voltage:      ${voltage.show}")      // "3.300387725344645 kg‧m^2/s^3‧A"
  println(s"Capacitance:  ${capacitance.show}")  // "1.272577754348972 s^4‧A^2/kg‧m^2"



  println()
  println("(d) Standard units shall exist as Scala types")
  println("(e) Standard units can be instantiated as values")

  val dKilos: kg  = 1.as[kg]
  val dMetres: m  = 1.as[m]
  val dSeconds: s = 1.as[s]
  val dAmps: A    = 1.as[A]
  val dTemp: K    = 1.as[K]
  val dMoles: mol = 1.as[mol]
  val dLum: cd    = 1.as[cd]

  println(dSeconds.show)
  println(dMetres.show)
  println(dKilos.show)
  println(dAmps.show)
  println(dTemp.show)
  println(dMoles.show)
  println(dLum.show)



  println()
  println("(f) Composed units can be created as types")  

  val yetAnotherSpeed: m/s = 2.as[m] / 4.as[s]
  println(yetAnotherSpeed.show) // "0.5 m/s"

  val farads: s*s*s*s*A*A/(kg*m*m) = capacitance 
  println(farads.show) // "1.272577754348972 s^4‧A^2/kg‧m^2"

  type speed = m/s

  val yetAnAdditionalSpeed: speed = (1.0/3.0).as[speed]
  println(yetAnAdditionalSpeed.show) // "0.3333333333333333 m/s"

  println("(f)(a) 1/s can be expressed as a type")  
  //val frequency: 1/s = 1 / 2.as[s]
  val frequency: one/s = 1 / 2.as[s]
  println(frequency.show) // "0.5 1/s"


 
  println()
  println("(g) Comparison between homogenous values must be natural")

  val lesserTemp  = 12.as[K]
  val greaterTemp = 373.2.as[K]
  val greatestTemp = 473.2.as[K]

  println(lesserTemp < greaterTemp) // "true"
  println(lesserTemp > greaterTemp) // "false"

  println(greaterTemp <= lesserTemp) // "false"
  println(greaterTemp <= greaterTemp) // "true" 
  println(greaterTemp <= greatestTemp) // "true"



  println()
  println("(h) Less-than/greater-than comparison between inconsistent values must be rejected at compile time")

  val aTemp = 8.as[K]
  val aCurrent = 2.as[A]

  // Compare Kelvin to Amperes: inconsistent comparison, compilation error
  // Message could be better, as the raw SIVec is exposed in the error
  // println(aTemp < aCurrent)



  println()
  println("(i) Equality between inconsistent values must be rejected at compile time")

  // Compare Kelvin to Amperes: inconsistent comparison, compilation error
  // println(aTemp == aCurrent) 

  // Compare Kelvin to Kelvin: consistent comparison
  println(lesserTemp == greaterTemp)



  println()
  println("(j) Doubles and integers should be implicitly convertable to dimensionless SI units and back")

  val radians: one = math.Pi / 2
  println(radians.show) // 1.5707963267948966 <1>

  val radToDeg: one = 180 / math.Pi

  val radAsDouble: Double = radians * radToDeg
  println(radAsDouble) // 90.0



  val degrees: one = 90
  println(degrees.show) // 90.0 <1>

  val degToRad: one = math.Pi / 180

  val degAsInt: Int = degrees * degToRad
  println(degAsInt) // 1
}
