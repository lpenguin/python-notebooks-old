package rosalind

object FastaReader {
  class FastaRecord(val name:String, val value: String)
  
  def readFasta(path:String):List[FastaRecord] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines()
    
    
    var name = ""
    val body = new scala.collection.mutable.StringBuilder()
    
    val res = scala.collection.mutable.ListBuffer[FastaRecord]()
    
    for(line <- lines){
      if(line.startsWith(">")){
        if(!name.isEmpty()){
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
}