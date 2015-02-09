package rosalind.util

import java.io.FileWriter
import java.io.File

import scala.collection.mutable.ListBuffer
import scala.io.Source
import rosalind.util.File._
object FastaReader {
  class FastaRecord(val name:String, val value: String){
    override def toString = ">"+name+": "+value
  }

  def fromData(name:String) = {
    fromFile(s"${getCurrentDirectory}/data/${name}.txt")
  }
  
  def fromFile(path:String):List[FastaRecord] = {
    fromLines(Source.fromFile(path).getLines())
  }

  def fromUrl(url:String) = {
    fromLines(Source.fromURL(url).getLines())
  }

  def fromLines(lines:Iterator[String]):List[FastaRecord] = {
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

    res.toList
  }

  def toFile(toFile:String, records:Seq[FastaRecord]): Unit ={
    val fw = new FileWriter(toFile)
    for(record <- records){
      fw.write(s">${record.name}\n")
      fw.write(record.value+"\n")
    }
    fw.close()
  }
}