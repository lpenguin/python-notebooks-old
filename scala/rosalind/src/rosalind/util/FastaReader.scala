package rosalind.util

import java.io.FileWriter

import scala.collection.mutable.ListBuffer

object FastaReader {
  class FastaRecord(val name:String, val value: String)
  
  def readFasta(path:String):List[FastaRecord] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines()
    
    
    var name = ""
    val body = new StringBuilder()
    
    val res = ListBuffer[FastaRecord]()
    
    for(line <- lines){
      if(line.startsWith(">")){
        if(!name.isEmpty){
          res += new FastaRecord(name, body.toString())      
        }
        name = line.tail
        body.clear()
      }else{
        body append line
      }
    }
    res += new FastaRecord(name, body.toString())      
    source.close()
    return res.toList
  }

  def writeFasta(toFile:String, records:Seq[FastaRecord]): Unit ={
    val fw = new FileWriter(toFile)
    for(record <- records){
      fw.write(s">${record.name}\n")
      fw.write(record.value+"\n")
    }
    fw.close()
  }
}