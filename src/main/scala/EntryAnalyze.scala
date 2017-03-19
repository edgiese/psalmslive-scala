import psalms.{Lectionary, LectionaryDay, PsalmEntry, PsalmSaver}

/**
  * Created by edgiese on 3/15/17.
  */
object EntryAnalyze {

  // main entry point
  def main(args: Array[String]): Unit = {
    val psalms = PsalmSaver.psalmsFromJsonFile("esv.json")
    val dayNumber = args(0).toInt

    val lect = new Lectionary()

    for (n <- 1 to 92) {
      val lectDay = lect.days(n)

      // sum words for an entry.  words <- lines <- verses <- psalmEntries
      def wordCount(entries: Seq[PsalmEntry]): Int = {
        entries.foldLeft(0)((sum, entry) => {
          val ps = psalms.getPiece(entry.number, entry.start, entry.end)
          sum + ps.verses.foldLeft(0)((s, v) => {
            s + v.lines.foldLeft(0)((sv, l) => {
              sv + l.text.split(" ").length
            })
          })
        })
      }

      val morningCount = wordCount(lectDay.morning)
      val eveningCount = wordCount(lectDay.evening)
      println (n + "," + morningCount + "," + eveningCount)
    }

    val lectDays = lect.days.toList
    lectDays.foreach (tuple => {
      print(tuple._1 + "  ")
      def printData(entries: Seq[PsalmEntry]): Unit = {
        entries.foreach(entry => {
          val piece = psalms.getPiece(entry.number, entry.start, entry.end)
        })


      }

    })
  }
}
