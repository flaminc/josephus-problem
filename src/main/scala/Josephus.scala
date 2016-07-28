
import scala.util.control.Exception.allCatch
import utils.MathUtils.PositiveMod

object Josephus {
  /**
    * Find the survivor's starting position after everyone else was eliminated.
    *
    * @param n The number of people in the circle.
    * @param k Step rate. After skipping `k - 1` people, the `k`th person will be eliminated.
    * @return The 0-based starting index of the last surviving person.
    */
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

  /**
    * Convenience method to cast command line arguments to a specified type and show errors on failures.
    *
    * @param argument   Argument from command line to parse/cast.
    * @param castFn     Function to use for casting. Should throw exception when parsing/casting fails.
    * @param failureMsg Message to use to clarify the error to the end user.
    * @tparam a Type to cast argument to.
    * @return Argument that was successfully casted to desired type `a`.
    */
  def argCast[a](argument: String, castFn: String => a, failureMsg: String): a = {
    allCatch.opt(castFn(argument)) match {
      case Some(value) => value
      case _ =>
        die(failureMsg, showUsage = true)
        throw new Throwable("Argument cast failure")
    }
  }

  /**
    * Kill the app and print `msg` provided to std error.
    *
    * @param msg       Description of why application is dieing to clarify to end user.
    * @param exitCode  Will be the return code emitted on application death. Defaults to 1.
    * @param showUsage Decides whether to show app usage on death.
    */
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
