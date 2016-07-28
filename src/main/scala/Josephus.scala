
import scala.util.control.Exception.allCatch
import utils.MathUtils.PositiveMod

object Josephus {

  def findSurvivor(n: Int, k: Int): Int = {
    if (n < 1) {
      throw new IllegalArgumentException("group size (n) must be 1 or greater")
    } else if (k < 1) {
      throw new IllegalArgumentException("step rate (k) must be 1 or greater")
    } else if (n == 1) {
      // base case of recursive calls
      0
    } else if (n < k || k == 1) {
      // generic case recursive method of solving
      (findSurvivor(n - 1, k) + k) % n
    } else {
      // n/k using integer math will simulate floor(n.toDouble/k)
      val p = Math.max(n - n / k, 1)
      // optimized case for when n >= k
      k * ((findSurvivor(p, k) - n % k) positiveMod p) / (k - 1)
    }

  }

  def main(args: Array[String]) {
    // validate arguments passed in to app
    if (args.length != 2) {
      die("App is missing exactly 2 arguments", showUsage = true)
    }
    val n = argCast(args(0), _.toInt, "argument 'n' must be an integer")
    val k = argCast(args(1), _.toInt, "argument 'k' must be an integer")

    // find the survivor and print result
    println(findSurvivor(n, k))

  }

  def argCast[a](argument: String, castFn: String => a, failureMsg: String): a = {
    allCatch.opt(castFn(argument)) match {
      case Some(value) => value
      case _ =>
        die(failureMsg, showUsage = true)
        throw new Throwable("Argument cast failure")
    }
  }

  def die(msg: String, exitCode: Int = 1, showUsage: Boolean = false): Unit = {
    Console.err.println(s"Application died with error: $msg")
    if (showUsage) {
      Console.err.println(
        """
          |Usage: app-name/run n k
          |
          | n - circle size
          | k - step rate
        """.stripMargin)
    }
    System.exit(exitCode)

  }
}
