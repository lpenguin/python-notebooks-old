package rosalind.util

object StringUtils {
  def overlaps(withSuffix:String, withPrefix:String, k:Int):Boolean = {
    if(withPrefix == withSuffix){
      false
    }else{
      val suffix = withSuffix.takeRight(k)
      val prefix = withPrefix.take(k)
      suffix == prefix
    }
  }

  def slidingFast(str:String, windowLen:Int):Iterator[String] = {
    new Iterator[String] {
      var pos = -1
      override def hasNext: Boolean = pos < str.size - windowLen

      override def next(): String = {
        pos += 1
        val res = str.substring(pos, pos + windowLen)
        res
      }
    }
  }

  def insertAtIndexes(str:String, pos:Seq[(Int, Char)]):String = {
    val array = str.toCharArray
    for((index, char) <- pos){
      array(index) = char
    }
    array.mkString
  }

  def letterDicts(dict:List[Char]) = (dict map (l => l -> dict.filterNot(_==l))).toMap

  def mutations(letterDicts:Map[Char, List[Char]], d:Int)(str:String):List[String] = {
    def possibleLetters(dicts:List[List[Char]]):List[List[Char]] = dicts match {
      case Nil => Nil
      case head::Nil => head map (List(_))
      case head::tail =>
        head flatMap { l =>
          possibleLetters(tail) map (l :: _)
        }
    }

    val res = (1 to d).toList flatMap { mc =>
      val possibleIndexes = 0 until str.size combinations mc
      possibleIndexes flatMap { indexes =>
        val dicts = indexes map (i => letterDicts(str(i)))
        possibleLetters(dicts.toList) map { letters =>
          insertAtIndexes(str, indexes zip letters)
        }
      }
    }
    str::res
  }

  def hamming(str1:String, str2:String):Int = {
    (str1 zip str2) count (t => t._1 != t._2)
  }

  def reverseComplement(str:String) = {
    str reverseMap {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'C' => 'G'
      case 'G' => 'C'
    }
  }
  
  implicit def StringUtilsExtension(s:String) = new {
    def overlaps(withPrefix:String, k:Int):Boolean = StringUtils.overlaps(s, withPrefix, k)
    def slidingFast(windowLen:Int):Iterator[String] = StringUtils.slidingFast(s, windowLen)
    def hamming(str:String):Int = StringUtils.hamming(s, str)
    def insertAtIndexes(pos:Seq[(Int, Char)]):String = StringUtils.insertAtIndexes(s, pos)
    def reverseComplement = StringUtils.reverseComplement(s)
  }
    
}