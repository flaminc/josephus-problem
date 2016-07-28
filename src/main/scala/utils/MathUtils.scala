package utils

object MathUtils {

  implicit class PositiveMod(dividend: Int) {
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
