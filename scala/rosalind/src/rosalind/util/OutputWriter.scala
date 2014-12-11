package rosalind.util

import java.io.FileWriter

/**
 * Created by nikita on 11.12.14.
 */
class OutputWriter(name:String, printlnResult:Boolean = false) {
  var fw = new FileWriter(s"./data/output_$name.txt")

  def writeln(line:String) = {
    fw.write(line+"\n")
    if(printlnResult){
      println(line)
    }
  }
  def close() = fw.close()
}
