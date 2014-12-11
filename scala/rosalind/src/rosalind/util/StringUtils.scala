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
  
  implicit def StringUtilsExtension(s:String) = new {
    def overlaps(withPrefix:String, k:Int):Boolean = StringUtils.overlaps(s, withPrefix, k)
  }
    
}