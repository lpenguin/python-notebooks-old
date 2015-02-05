val str = """img src="/media/attachments/lessons/199/gluing_gg_1.png" img src="/media/attachments/lessons/199/gluing_gg_1.png""""
val imgRe = """img src="(.*)"""".r

str replaceAll ("""img src="(.*)"""", "img src=\"$1")
str match  {
  case imgRe(url) => s"""img src="http://stepic.org$url""""
}

