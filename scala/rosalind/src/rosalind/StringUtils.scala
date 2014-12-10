package rosalind

object StringUtils {
  def overlaps(withSuffix:String, withPrefix:String, k:Int):Boolean = {
      val suffix = withSuffix.takeRight(k)
      val prefix = withPrefix.take(k)
      return suffix == prefix 
  }
  
  implicit def StringUtilsExtension(s:String) = new {
    def overlaps(withPrefix:String, k:Int):Boolean = StringUtils.overlaps(s, withPrefix, k)
  }
    
}