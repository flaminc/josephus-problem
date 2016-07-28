package utils

import org.specs2.Specification
import utils.MathUtils.PositiveMod

/**
  * Created by flaming on 7/28/2016.
  */
class MathUtilsSpec extends Specification { def is = s2"""
Math Utils testing
  Positive dividend should match % operator results     $positive
  Negative dividend should result in a positive result  $negative
  0 dividend                                            $dividend0
  0 divisor                                             $divisor0
  """

  def positive = 7 positiveMod 3 must_== 1

  def negative = -7 positiveMod 3 must_== 2

  def dividend0 = 0 positiveMod 3 must_== 0

  def divisor0 = 7 positiveMod 0 must throwAn[Exception]
}
