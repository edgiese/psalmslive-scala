import java.io.{File, PrintWriter}

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.model._
import psalms._
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._

/**
  * scraper routines for reading psalm structure from Bible Gateway's format
  */
object EntryScrape {
  // utility routine to print scraper's sub elements for an element
  def printAll(element: Element): Unit = {
    element.childNodes.foreach {
      case t: TextNode => print(t.content)
      case n: ElementNode => {
        val e = n.element
        val discriminator = e.tagName + "." + e.head.attrs.getOrElse("class", "none").split(" ")(0)
        print(f"<$discriminator>")
        printAll(n.element)
        if (e.tagName != "br")
          println(f"</${e.tagName}>")
      }
    }
  }

  // add text to the end of a verse, i.e., to the last line of the verse
  def appendToVerse(oldVerse: Verse, text: String): Verse = {
    if (oldVerse.lines.isEmpty) {
      // create a new line with the text in it
      Verse(oldVerse.verseNum, oldVerse.lines :+ VerseLine(0,sanitize(text),0))
    } else {
      val oldLine = oldVerse.lines.last
      Verse(oldVerse.verseNum, oldVerse.lines.init :+ VerseLine(oldLine.indent, oldLine.text + sanitize(text), 0))
    }
  }

  // builds up a verse from its nodes
  // primary scraper routine
  def addVerseNodes(nodes: Iterable[Node], accum: List[Verse], lastVerse: Verse): (List[Verse], Verse) = {
    if (nodes.isEmpty)
      (accum, lastVerse)
    else {
      val node = nodes.head
      node match {
        case t: TextNode =>
          addVerseNodes(nodes.tail, accum, appendToVerse(lastVerse, t.content))
        case n: ElementNode =>
          val e = n.element
          val discriminator = e.tagName + "." + e.head.attrs.getOrElse("class", "none").split(" ")(0)
          discriminator match {
            case "span.chapternum" =>
              // always verse 1.  start a new verse
              addVerseNodes(nodes.tail, accum, Verse(1, lastVerse.lines))
            case "sup.versenum" =>
              // start of a new verse
              val newVerseNum = e.text.substring(0, e.text.length-1).toInt
              // if we had an empty line to receive more text, and it was never used, drop it here
              val adjustedLastVerse = if (lastVerse.lines.nonEmpty && lastVerse.lines.last.text.isEmpty) {
                Verse(lastVerse.verseNum, lastVerse.lines.init)
              } else {
                lastVerse
              }
              addVerseNodes(nodes.tail, accum :+ adjustedLastVerse, Verse(newVerseNum, List()))
            case "span.indent-1" =>
              val indentedLastVerse =
                if (lastVerse.lines.isEmpty)
                  Verse(lastVerse.verseNum, lastVerse.lines :+ VerseLine(1, "", 0))
                else
                  Verse(lastVerse.verseNum, lastVerse.lines.init :+ VerseLine(1, "", 0))
              val (newAccum, newLastVerse) = addVerseNodes(e.childNodes, accum, indentedLastVerse)
              addVerseNodes(nodes.tail, newAccum, newLastVerse)
            case "span.small-caps" =>
              // we can assume small caps is LORD
              addVerseNodes(nodes.tail, accum, appendToVerse(lastVerse, "LORD"))
            case "sup.footnote" =>
              // all footnotes are just asterisks
              addVerseNodes(nodes.tail, accum, appendToVerse(lastVerse, "*"))
            case "br.none" =>
              if (lastVerse.lines.isEmpty) {
                addVerseNodes(nodes.tail, accum, Verse(lastVerse.verseNum, List(VerseLine(0, "", 0))))
              } else {
                val last = lastVerse.lines.last
                if (last.text.nonEmpty) {
                  // existing text:  terminate line
                  addVerseNodes(nodes.tail, accum, Verse(lastVerse.verseNum, lastVerse.lines :+ VerseLine(0, "", 0)))
                } else {
                  // add space to last line
                  addVerseNodes(nodes.tail, accum, Verse(lastVerse.verseNum, lastVerse.lines.init :+ VerseLine(last.indent, "", last.after + 1.0)))
                }
              }
            case "span.indent-1-breaks" => // ignore this
              addVerseNodes(nodes.tail, accum, lastVerse)
            case _ =>
              val (newAccum, newLastVerse) = addVerseNodes(e.childNodes, accum, lastVerse)
              addVerseNodes(nodes.tail, newAccum, newLastVerse)
          }
      }
    }
  }

  // bible gateway has a <p.line> for groups of verses.  All of them have essentially a blank line after them
  // this routine scans these <p.line>s.
  def scanPLines(pLines: List[Element], accum: List[Verse], lastVerse: Verse): (List[Verse], Verse) = {
    if (pLines.isEmpty) {
      (accum, lastVerse)
    } else {
      val pLine = pLines.head
      /*
      println("=================================================")
      printAll(element)
      println("-------------------------------------------------")
      */
      val (newAccum, newLastVerse) = addVerseNodes(pLine.childNodes, accum, lastVerse)
      // add a blank line to the end of every p.line
      // note -- this will throw an exception for an empty p.line, but these do not appear to happen
      val last = newLastVerse.lines.last
      val withBlank = Verse(newLastVerse.verseNum, newLastVerse.lines.init :+ VerseLine(last.indent, last.text, last.after + 1.0))
      scanPLines(pLines.tail, newAccum, withBlank)
    }
  }

  // wrapper routine for scanPLines
  def scanVerses(pLines: List[Element]): List[Verse] = {
    val lastVerse = Verse(0, List())
    val (calcVerses, calcLastVerse) = scanPLines(pLines, List(), lastVerse)
    // if there was a fragment at the end, add it on
    if (calcLastVerse.verseNum > 0) {
      calcVerses :+ calcLastVerse
    } else {
      calcVerses
    }
  }

  def sanitize(text: String): String = {
    val xlate = Seq(
      "\u201c" -> "\"",
      "\u201d" -> "\"",
      "\u2018" -> "'",
      "\u2019" -> "'",
      "\u2014" -> "--",
      "\u2026" -> "...",
      "\u2013" -> "-"
    )
    xlate.foldLeft(text){case (t, (from, to)) => t.replace(from, to)}
  }

  def fnItemText(element: Element): String = {
    element.childNodes.map {
      case t: TextNode => sanitize(t.content)
      case n: ElementNode =>
        val e = n.element
        if (e.tagName == "i")
          '<' + sanitize(fnItemText(e)) + '>'
        else if (e.tagName == "a" && e.attrs.contains("title") && e.attr("title").startsWith("Go to"))
          ""
        else
          fnItemText(e)
    }.foldLeft("")(_ + _).trim
  }

  // open a file and scrape a psalm -- return it in internal psalm format
  def scrapePsalm(doc: Document, psalmNumber: Int): Psalm = {
    def firstText(elements: List[Element]): String = if (elements.isEmpty) "" else elements.head.text
    val OVERALL_DIV = "div.result-text-style-normal"

    // technically, we could go through and parse these things as html, but since this is a special purpose
    // scraper, we will just pull the text and do some hardcoded replaces.
    val descriptionItems = doc >> elementList(OVERALL_DIV + " h4.psalm-title")
    val description = if (descriptionItems.isEmpty) "" else {
      val myList = List(descriptionItems.head)
      sanitize(firstText(myList).replace("[a]", "*").replace("[b]", "*").replace("Lord", "LORD"))
    }

    val verseElements = doc >> elementList(OVERALL_DIV + " p.line")
    val verses = scanVerses(verseElements)

    val footnoteItems = doc >> elementList(OVERALL_DIV + " div.footnotes ol li")
    val footnotes = if (footnoteItems.isEmpty)
      List[String]()
    else
      footnoteItems.map(element => fnItemText(element))

    Psalm(
      psalmNumber,
      description,
      verses,
      footnotes
    )
  }

  // main entry point
  def main(args: Array[String]): Unit = {
    val baseFileName = "/home/edgiese/psalms"
    val browser = JsoupBrowser()
    val psalms = for (n <- 1 to 150) yield {
      val doc = browser.parseFile(baseFileName + f"/psalm$n%03d.html")
      val ps = scrapePsalm(doc, n)
      println(ps)
      println("---------------")
      ps
    }
    val json = psalmsToJson(psalms.toList)
    val writer = new PrintWriter(new File("test.json"))
    writer.write(pretty(render(json)))
    writer.close()
  }

  // entry to save the psalms into files
  def savePsalm(baseSaveName: String, psalm: Int): Unit = {
    val browser = JsoupBrowser()
    val doc = browser.get("https://www.biblegateway.com/passage/?search=Psalm%20" + psalm + "&version=ESV/")
    val writer = new PrintWriter(new File(baseSaveName + f"/$psalm%03d.html"))
    writer.write(doc.toHtml)
    writer.close()
  }

  def psalmsToJson(psalms: List[Psalm]): JObject = {
    "psalms" -> psalms.map(ps =>
      ("number" -> ps.number) ~
        ("description" -> ps.description) ~
        ("verses" -> ps.verses.map(v =>
          ("number" -> v.verseNum) ~
            ("lines" -> v.lines.map(line =>
              ("indent" -> line.indent) ~
                ("text" -> line.text) ~
                ("after" -> line.after)
              )
            )
          )
        ) ~
        ("footnotes" -> ps.footnotes)
    )
  }
}


