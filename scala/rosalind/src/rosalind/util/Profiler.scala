package rosalind.util

/**
 * Created by nikita on 28.12.14.
 */
import System.{currentTimeMillis => _time}
object Profiler{
  def profile[R](code: => R, t: Long = _time) = {
    code
    _time - t
  }
}
