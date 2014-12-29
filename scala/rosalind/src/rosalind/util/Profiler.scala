package rosalind.util

/**
 * Created by nikita on 28.12.14.
 */
import System.{currentTimeMillis => _time}
object Profiler{
  private val mb = 1024*1024

  def profile[R](code: => R, t: Long = _time) = {code; _time - t}

  def traceMem() = {
    val runtime = Runtime.getRuntime
    println("Used Memory: " + (runtime.totalMemory() - runtime.freeMemory()) / mb +" MB")
    println("Free Memory: " + runtime.freeMemory() / mb +" MB")
    println("Total Memory: " + runtime.totalMemory() / mb +" MB")
    println("Max Memory: " + runtime.maxMemory() / mb +" MB")
  }
}
