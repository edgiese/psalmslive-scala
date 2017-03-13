package psalms

/**
  * A simple structure for representing the Psalms
  */

// blank inherited trait to allow the following items to share lists
trait VerseLine

// line with text in it.  can be indented
case class line(indent: Int, text: String) extends VerseLine
{
  override def toString: String = (" " * 2 * indent) + text + "\r\n"
}

// blank line
case class blankLine() extends VerseLine
{
  override def toString: String = "\r\n"
}

// subheading -- used only in psalm 119
case class subHeading(text: String) extends VerseLine
{
  override def toString: String = text
}

// a verse consists of potentially multiple lines
case class verse(verseNum: Int, lines: List[VerseLine])
{
  override def toString: String = {
    if (lines.isEmpty)
      ""
    else
      lines.tail.foldLeft(f"$verseNum%3d  ${lines.head.toString}")(_ + "     " + _.toString)
  }
}

// the overall psalm
class Psalm(
             number: Int,              // the number, 1-150
             description: String,      // optional description, "verse 1" in masoretic text, but unnumbered in LXX
             verses: List[verse],      // list of verses (see above structure)
             footnotes: List[String])  // list of footnotes, could be empty
{
  override def toString: String = {
    "Psalm " + number + "\r\n" +
      description + "\r\n" +
      verses.foldLeft("")(_ + _.toString) +
      (if (footnotes.isEmpty) "" else footnotes.fold("\r\nFootnotes:\r\n")(_ + _ + "\r\n"))
  }
}

