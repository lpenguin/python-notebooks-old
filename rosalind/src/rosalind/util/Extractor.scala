package rosalind.util

/**
 * Created by nikitaprianichnikov on 09.02.15.
 */
object Extractor {
  object Extract{
    def unapplySeq(string: String):Option[List[String]] =
      try{ Some(string split "\\s+" toList) }
      catch{ case _: Throwable => None}
  }

  object Int{
    def unapply(string: String):Option[Int] =
      try { Some(string.toInt) }
      catch { case _:Throwable => None}
  }
}
