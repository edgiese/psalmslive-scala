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
}

