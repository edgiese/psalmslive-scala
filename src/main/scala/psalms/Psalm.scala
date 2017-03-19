package psalms

/**
  * A simple structure for representing the Psalms
  */

case class VerseLine(indent: Int, text: String, after: Double)
{
  override def toString: String = (" " * 2 * indent) + text + ("\r\n" * math.floor(1.5 + after).toInt)
}

// a verse consists of potentially multiple lines
case class Verse(verseNum: Int, lines: List[VerseLine])
{
  override def toString: String = {
    if (lines.isEmpty)
      ""
    else
      lines.tail.foldLeft(f"$verseNum%3d  ${lines.head.toString}")(_ + "     " + _.toString)
  }

}

// the overall psalm
case class Psalm(
                  number: Int, // the number, 1-150
                  description: String, // optional description, "verse 1" in masoretic text, but unnumbered in LXX
                  verses: List[Verse], // list of verses (see above structure)
                  footnotes: List[String])  // list of footnotes, could be empty
{
  override def toString: String = {
    "Psalm " + number + "\r\n" +
      description + "\r\n" +
      verses.foldLeft("")(_ + _.toString) +
      (if (footnotes.isEmpty) "" else footnotes.fold("Footnotes:\r\n")(_ + _ + "\r\n"))
  }

  // count number of footnote anchors in a string
  protected def anchorCount(text: String): Int = text.count(_ == '*')

  // gets a partial psalm from a range of verses.  Verse numbers are 1-based.
  def getPiece(startVerse: Int, endVerse: Int): Psalm = {

    // internal function to build verse list of subset
    // returns (verseList, nSkipAnchors, nAnchorCount
    def buildPiece(
                    verses: List[Verse],
                    accum: List[Verse],
                    nSkipAnchors: Int,
                    nAnchorCount: Int
                  ): (List[Verse], Int, Int) = {
      if (verses.isEmpty)
        (accum, nSkipAnchors, nAnchorCount)
      else {
        val verse = verses.head
        if (verse.verseNum < startVerse)
          buildPiece(verses.tail, accum, nSkipAnchors + verse.lines.map(line => anchorCount(line.text)).sum, nAnchorCount)
        else if (verse.verseNum <= endVerse)
          buildPiece(verses.tail, accum :+ verse, nSkipAnchors, nAnchorCount + verse.lines.map(line => anchorCount(line.text)).sum)
        else
          (accum, nSkipAnchors, nAnchorCount)
      }
    }
    // only include the description if this is the first portion of the psalm. Otherwise, skip over the intro footnotes
    val startAnchorCount = if (startVerse > 1) anchorCount(description) else 0
    val (verseSubset, nSkipAnchors, nVerseAnchorCount) = buildPiece(verses, List(), startAnchorCount, 0)
    val nAnchorCount = if (startVerse > 1) nVerseAnchorCount else nVerseAnchorCount + anchorCount(description)
    Psalm(
      number,
      if (startVerse > 1) "" else description,
      verseSubset,
      if (nAnchorCount > 0) footnotes.slice(nSkipAnchors, nSkipAnchors + nAnchorCount) else List()
    )
  }
}

// the overall Psalms class is just a set of all the psalms in a particular translation
case class Psalms(translation: String, psalms: List[Psalm]) {
  // allow to fail if we can't find ps number
  def get(number: Int): Option[Psalm] = psalms.find(ps => ps.number == number)

  // get a piece of a psalm -- throws exception if psalm not found
  def getPiece(number: Int, startVerse: Int, endVerse: Int): Psalm = {
    val ps = this.get(number).get
    ps.getPiece(startVerse, endVerse)
  }
}

