import java.io.{File, PrintWriter}

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.model._
import psalms._

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

  def appendToVerse(oldVerse: verse, text: String): verse = {
    if (oldVerse.lines.isEmpty) {
      // create a new line with the text in it
      verse(oldVerse.verseNum, oldVerse.lines :+ line(0,text))
    } else {
      oldVerse.lines.last match {
        case l: line =>
          // extending line with text
          verse(oldVerse.verseNum, oldVerse.lines.init :+ line(l.indent, l.text + text))
        case bl: blankLine => // replace blank line with non-blank line
          verse(oldVerse.verseNum, oldVerse.lines.init :+ line(0,text))
        case _ =>
          // appending a new line to the array.  Note that this will sometimes create empty lines
          verse(oldVerse.verseNum, oldVerse.lines :+ line(0,text))
      }
    }
  }

  // builds up a verse from its nodes
  def addVerseNodes(nodes: Iterable[Node], accum: List[verse], lastVerse: verse): (List[verse], verse) = {
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
              addVerseNodes(nodes.tail, accum, verse(1, lastVerse.lines))
            case "sup.versenum" =>
              // start of a new verse
              val newVerseNum = e.text.substring(0, e.text.length-1).toInt
              // if we had an empty line to receive more text, and it was never used, drop it here
              val adjustedLastVerse = if (lastVerse.lines.nonEmpty) {
                lastVerse.lines.last match {
                  case l: line =>
                    if (l.text.isEmpty)
                      verse(lastVerse.verseNum, lastVerse.lines.init)
                    else
                      lastVerse
                  case _ =>
                    lastVerse
                }
              } else
                lastVerse
              addVerseNodes(nodes.tail, accum :+ adjustedLastVerse, verse(newVerseNum, List()))
            case "span.indent-1" =>
              val indentedLastVerse =
                if (lastVerse.lines.isEmpty)
                  verse(lastVerse.verseNum, lastVerse.lines :+ line(1, ""))
                else
                  verse(lastVerse.verseNum, lastVerse.lines.init :+ line(1, ""))
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
                addVerseNodes(nodes.tail, accum, verse(lastVerse.verseNum, List(blankLine())))
              } else {
                lastVerse.lines.last match {
                  case b: blankLine =>
                    addVerseNodes(nodes.tail, accum, verse(lastVerse.verseNum, lastVerse.lines :+ blankLine()))
                  case l: line =>
                    if (l.text.isEmpty) {
                      addVerseNodes(nodes.tail, accum, verse(lastVerse.verseNum, lastVerse.lines.init :+ blankLine()))
                    } else {
                      addVerseNodes(nodes.tail, accum, verse(lastVerse.verseNum, lastVerse.lines :+ line(0, "")))
                    }
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
  def scanPLines(pLines: List[Element], accum: List[verse], lastVerse: verse): (List[verse], verse) = {
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
      val withBlank = verse(newLastVerse.verseNum, newLastVerse.lines :+ blankLine())
      scanPLines(pLines.tail, newAccum, withBlank)
    }
  }

  // wrapper routine for scanPLines
  def scanVerses(pLines: List[Element]): List[verse] = {
    val lastVerse = verse(0, List())
    val (calcVerses, calcLastVerse) = scanPLines(pLines, List(), lastVerse)
    // if there was a fragment at the end, add it on
    if (calcLastVerse.verseNum > 0) {
      calcVerses :+ calcLastVerse
    } else {
      calcVerses
    }
  }

  def fnItemText(element: Element): String = {
    element.childNodes.map {
      case t: TextNode => t.content
      case n: ElementNode =>
        val e = n.element
        if (e.tagName == "i")
          '"' + fnItemText(e) + '"'
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
    // scraper.
    val descriptionItems = doc >> elementList(OVERALL_DIV + " h4.psalm-title")
    val description = if (descriptionItems.isEmpty) "" else {
      val myList = List(descriptionItems.head)
      firstText(myList).replace("[a]", "*").replace("Lord", "LORD")
    }

    val verseElements = doc >> elementList(OVERALL_DIV + " p.line")
    val verses = scanVerses(verseElements)

    val footnoteItems = doc >> elementList(OVERALL_DIV + " div.footnotes ol li")
    val footnotes = if (footnoteItems.isEmpty)
      List[String]()
    else
      footnoteItems.map(element => fnItemText(element))

    new Psalm(
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
    for (n <- 1 to 150) {
      val doc = browser.parseFile(baseFileName + f"/psalm$n%03d.html")
      println(scrapePsalm(doc, n))
    }; println("---------------")
  }

  // entry to save the psalms into files
  def savePsalm(baseSaveName: String, psalm: Int): Unit = {
    val browser = JsoupBrowser()
    val doc = browser.get("https://www.biblegateway.com/passage/?search=Psalm%20" + psalm + "&version=ESV/")
    val writer = new PrintWriter(new File(baseSaveName + f"/$psalm%03d.html"))
    writer.write(doc.toHtml)
    writer.close()
  }
}
