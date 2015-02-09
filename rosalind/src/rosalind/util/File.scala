package rosalind.util

import java.io.File

/**
 * Created by nikita on 18.12.14.
 */
object File {
  def getCurrentDirectory = new File( "." ).getCanonicalPath
}
