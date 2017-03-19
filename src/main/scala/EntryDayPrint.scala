import psalms.{Lectionary, PsalmEntry, PsalmSaver}

/**
  * Created by edgiese on 3/15/17.
  */
object EntryDayPrint {
  // main entry point
  def main(args: Array[String]): Unit = {
    val psalms = PsalmSaver.psalmsFromJsonFile("esv.json")
    val lect = new Lectionary()

    val n = args(0).toInt

    val lectDay = lect.days(n)

    // sum words for an entry.  words <- lines <- verses <- psalmEntries
    def printEntries(title: String, entries: Seq[PsalmEntry]): Unit = {
      println("DAY " + n + " " + title)
      entries.foreach(entry => {
        val ps = psalms.getPiece(entry.number, entry.start, entry.end)
        println(ps)
      })
    }
    printEntries("MORNING", lectDay.morning)
    println("--------------------------------")
    printEntries("EVENING", lectDay.evening)
  }
}
