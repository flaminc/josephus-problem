package utils

/**
  * Provide some basic math utils
  */
object MathUtils {

  implicit class PositiveMod(dividend: Int) {
    /**
      * Customize modulus operator `%` so that when the dividend is - the remainder is positive. No guarantees will be
      * made about the divisor being negative.
      *
      * @param divisor The divisor in the division.
      * @return Positive remainder of division.
      */
    def positiveMod(divisor: Int): Int = {
      // Math.floorMod could have been used but that would peg java at 1.8
      if (dividend < 0) {
        dividend % divisor + divisor
      } else {
        dividend % divisor
      }
    }
  }
}
