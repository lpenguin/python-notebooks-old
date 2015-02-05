package stepicdownload

import java.io.FileOutputStream

import io.github.cloudify.scala.spdf._
import org.json4s.JValue
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

import scala.io.Source
import java.net.URL
import java.io.File
/**
 * Created by nikita on 05.02.15.
 */
object Main {
  val html =
    """
      |<p><img src="/media/attachments/lessons/199/gluing_gg_1.png" alt="Figure" style="width: 90%;" class="img_special   shrink  ">
      |</p>
      |<p><span style="font-size:0.85em"><strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Figure:</strong> Bringing the two nodes labeled <strong><span style="color:forestgreen"><span class="monospace">GG</span></span>
      |  </strong> closer (left) and closer (middle) to each other to eventually glue
      |  <br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;them into a single node (right). The path with sixteen nodes has been transformed into the graph
      |  <br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<em>DeBruijn</em><sub>3</sub>(<strong><span class="monospace">TA<span style="color:royalblue">ATG</span><span style="color:crimson">CC</span><span style="color:royalblue">ATG</span><span style="color:forestgreen">GG</span><span style="color:royalblue">ATG</span><span style="color:purple">TT</span></span></strong>),
      |  which has eleven nodes.</span>
      |</p>
      |
    """.stripMargin
  def main(args: Array[String]) {


//    val outputStream = new FileOutputStream("/home/nikita/tmp/foo.pdf")
//    pdf.run(html, outputStream)
//    outputStream.close()
    val csrfToken = "lxi8jpDRlNfvkXxITGa1AaaORscyS33e"
    val sessionId = "9o9pk8nv9phvwpq9pg8cfks3qf5ormzb"
    val cSRFData = new CSRFData(csrfToken, sessionId)

    val sections = getSections(List(108, 109, 110, 112, 113), cSRFData)

    for((section, i) <- sections.zipWithIndex){
      val dirName = s"/home/nikita/tmp/${i+1} - ${section.name}"
      println(s"Processing section: ${section.name}")
      new File(dirName).mkdirs()
      for((lesson, j) <- section.lessions.filterNot(l => l.name != "INVALID").zipWithIndex){
        println(s"Processing lesson: ${lesson.name}")
        val fileName = s"$dirName/${j+1} - ${lesson.name}"
        val stepsHtml = parseSteps(lesson.steps, cSRFData)
        renderLesson(lesson, stepsHtml, fileName)
      }
    }

//    val l = lesson(199, cSRFData)
//    val stepsHtml = parseSteps(l.steps, cSRFData)
//
//    renderLesson(l, stepsHtml, "/home/nikita/tmp/foo.pdf")
  }

  class CSRFData(val token:String, val sessionId:String)
  class Lesson(val name:String, val steps:Seq[Int])
  class Section(val name:String, val lessions:Seq[Lesson])

  def getLesson(lessonId:Int, cSRFData: CSRFData):Lesson = {
    var url = s"https://stepic.org/api/lessons/$lessonId"
    val data = fromCSRFURL(url, cSRFData)
    val json = parse(data)
    val lesson = (json \ "lessons")(0)
    val steps = for{JInt(s) <- lesson \ "steps"} yield s.toInt
    val JString(title) = lesson \ "title"

    new Lesson(title, steps)
  }

  def getSections(sectionIds:Seq[Int], cSRFData: CSRFData):Seq[Section] = {
    def section(sectionJson:JValue):Section = {
      def mkLesson(l:JObject):Lesson = {
        val steps = for{JInt(s) <- l \ "steps"} yield s.toInt
        val titleJson = l \ "title"
        if(titleJson != JNothing){
          val JString(title) = titleJson
          new Lesson(title, steps)
        }else{
          new Lesson("INVALID", steps)
        }

      }

      val JString(name) = (sectionJson \ "sections")(0) \ "title"
      val lessons = for{JObject(lesson) <- sectionJson \ "lessons"} yield mkLesson(lesson)

      new Section(name, lessons)
    }

    sectionIds map { sectionId =>
      val data = fromCSRFURL("https://stepic.org/api/sections?ids[]="+sectionId, cSRFData)
      section(parse(data))
    }
  }

  def fromCSRFURL(url:String, cSRFData: CSRFData):String = {
    val requestProperties = Map(
      "User-Agent" -> "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:35.0) Gecko/20100101 Firefox/35.0",
      "X-CSRFToken" -> cSRFData.token,
      "Accept" -> "application/json, text/javascript, */*; q=0.01",
      "X-Requested-With" -> "XMLHttpRequest",
      "Cookie" -> s"csrftoken=${cSRFData.token}; sessionid=${cSRFData.sessionId};"
    )


    val connection = new URL(url).openConnection
    requestProperties.foreach({
      case (name, value) => connection.setRequestProperty(name, value)
    })

    Source.fromInputStream(connection.getInputStream).mkString
  }

  def renderLesson(lesson: Lesson, steps:Seq[String], outFile:String):Unit = {
    val html = "<h1>"+lesson.name+"</h1>"+(steps mkString "<hr/>")

    val pdf = Pdf(new PdfConfig {
      orientation := Landscape
      pageSize := "Letter"
      marginTop := "1in"
      marginBottom := "1in"
      marginLeft := "1in"
      marginRight := "1in"
    })

    val outputStream = new FileOutputStream(outFile)
    pdf.run(html, outputStream)
    outputStream.close()
  }

  def parseSteps(steps:Seq[Int], cSRFData: CSRFData):Seq[String] = {
    val stepsUri = "https://stepic.org/api/steps?"
    val args = steps map ("ids[]="+_) mkString "&"

    val data = fromCSRFURL(stepsUri+args, cSRFData)
    println(stepsUri+args)

    val json = parse(data)
    println(data)
    val stepsJson = json \ "steps"
    for {JObject(stepJson) <- stepsJson
         JField("block", JObject(block)) <- stepJson
         JField("text", JString(text)) <- block
    } yield preprosImg(text)
  }

  def preprosImg(html:String):String = {
    val img = html replaceAll ("""img src="(.*)"""", "img src=\"https://stepic.org$1\"")
    img  replaceAll ("""a href="(.*)"""", "a href=\"https://stepic.org$1\"")
  }
}
