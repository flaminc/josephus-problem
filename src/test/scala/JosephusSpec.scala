import org.specs2.Specification

class JosephusSpec extends Specification { def is = s2"""
Various tests for counting out problem

  Invalid n parameter should throw                  $nInvalidParameter
  Invalid k parameter should throw                  $kInvalidParameter
  What happens if n = k?                            $nEqualsK
  What happens if n = 3 and k = 2?                  $n3k2
  What happens if n is very large, but k = 2?       $nLarge_k2
  k can be larger then n (n=4 k=8 -> 2)             $largeK
  Base case for group of 1 should return 0          $baseCase
  Single stepping should give last index in circle  $singleStep
  """

  def nInvalidParameter = Josephus.findSurvivor(n = 0, k = 1) must throwAn[IllegalArgumentException]

  def kInvalidParameter = Josephus.findSurvivor(n = 10, k = 0) must throwAn[IllegalArgumentException]

  def nEqualsK = Josephus.findSurvivor(n = 4, k = 4) must_== 1

  def n3k2 = Josephus.findSurvivor(n = 3, k = 2) must_== 2

  // there is an emergent pattern when k=2 that makes the position easy to know. The pattern iterates through odd numbers
  // (for 1 based index numbers) resetting to 1 at every point where n is 2^X power.
  // 2^10 + 1 would give 3 or 2 for 0 based indexing.
  def nLarge_k2 = Josephus.findSurvivor(n = math.pow(2, 10).toInt + 1, k = 2) must_== 2

  def largeK = Josephus.findSurvivor(4, 8) must_== 2

  def baseCase = Josephus.findSurvivor(1, 4) must_== 0

  def singleStep = Josephus.findSurvivor(20, 1) must_== 19

}
