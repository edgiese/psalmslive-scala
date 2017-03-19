package psalms

import java.io.PrintWriter

import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.DefaultFormats

import scala.reflect.io.File

/**
  * Created by edgiese on 3/15/17.
  */
object PsalmSaver {
  def psalmsToJson(psalms: Psalms): JObject = {
    ("translation" -> psalms.translation) ~
    ("psalms" -> psalms.psalms.map(ps =>
      ("number" -> ps.number) ~
        ("description" -> ps.description) ~
        ("verses" -> ps.verses.map(v =>
          ("verseNum" -> v.verseNum) ~
            ("lines" -> v.lines.map(line =>
              ("indent" -> line.indent) ~
                ("text" -> line.text) ~
                ("after" -> line.after)
            )
              )
        )
          ) ~
        ("footnotes" -> ps.footnotes)
    ))
  }

  // utility for closeables
  protected def using[A, B <: {def close(): Unit}](closeable: B)(f: B => A): A = {
    try {
      f(closeable)
    } finally {
      closeable.close()
    }
  }

  def psalmsToJsonFile(pss: Psalms, fileName: String): Unit = {
    val json = psalmsToJson(pss)
    using(new PrintWriter(fileName))(writer => writer.write(pretty(render(json))))
  }

  def psalmsFromJsonFile(fileName: String): Psalms = {
    val json =
        using(File(fileName).inputStream) { stream =>
          parse(stream)
        }
    implicit val formats = DefaultFormats
    json.extract[Psalms]
  }

}
