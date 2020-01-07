package day4

import scala.collection.immutable.SortedSet
import scala.util.matching.Regex

object Repose {

  def stringsToRecords(strings: Seq[String]): Map[String, Seq[Range]] = {
    val dateTimeGroup: String = raw"\[\d{4}-\d{2}-\d{2} \d{2}:(\d{2})\]"
    val beginGroup: String = raw"Guard #(\d+) begins shift"
    val asleepGroup: String = "falls asleep"
    val wakesGroup: String = "wakes up"
    val beginLineReg: Regex =
      (dateTimeGroup + " " + beginGroup).r("minute", "id")
    val asleepLineReg: Regex = (dateTimeGroup + " " + asleepGroup).r("minute")
    val wakesLineReg: Regex = (dateTimeGroup + " " + wakesGroup).r("minute")
    var latestID: Option[String] = None
    var latestSleep: Option[Int] = None
    val sortedStrings: SortedSet[String] = SortedSet.from(strings.toSet)
    var records: Seq[(String, Range)] = Seq.empty
    sortedStrings.foreach((s: String) => {
      if (beginLineReg.matches(s)) {
        latestID = Some(beginLineReg.findAllIn(s).group("id"))
      }
      else if (asleepLineReg.matches(s)) {
        if (latestID.isEmpty)
          throw new IllegalStateException("No ID found before asleep: " + s)
        else if (latestSleep.isDefined) {
          throw new IllegalStateException(
            "New sleep found before previous wakes: " + s
          )
        }
        else
          latestSleep = Some(asleepLineReg.findAllIn(s).group("minute").toInt)
      }
      else if (wakesLineReg.matches(s)) {
        if (latestSleep.isEmpty)
          throw new IllegalStateException("No asleep found before wake: " + s)
        else {
          val latestWake: Int = wakesLineReg.findAllIn(s).group("minute").toInt
          records = records :+ (latestID.get, Range(latestSleep.get, latestWake))
          latestSleep = None
        }
      }
      else
        throw new IllegalArgumentException("Unable to parse string: " + s)
    })
    if (latestSleep.isDefined)
      throw new IllegalStateException("No wake found at end.")

    records.groupMap(_._1)((r: (String, Range)) => r._2)
  }

  def recordsToMinsSleeping(records: Map[String, Seq[Range]]): Set[(String, Int)] = {
    records.toSet.map((record: (String, Seq[Range])) =>
      record match {
        case (id, naps) =>
          val durations: Seq[Int] = naps.map((r: Range) => r.size)
          (id, durations.sum)
    })
  }

  def idOfGuardSleepingMost(records: Map[String, Seq[Range]]): String = {
    recordsToMinsSleeping(records).maxBy(_._2)._1
  }

  def numberAppearingMostWithCount(naps: Seq[Range]): (Int, Int) = {
    var minutes: Seq[Int] = Seq.empty
    naps.foreach((nap: Range) => minutes = minutes ++ (for (n <- nap) yield n))
    val maxMinutes: (Int, Seq[Int]) = minutes.groupBy((i: Int) => i).maxBy(_._2.size)
    (maxMinutes._1, maxMinutes._2.size)
  }

  def recordsToMostNapsOneMinute(records: Map[String, Seq[Range]]): Set[(String, Int, Int)] = {
    records.toSet.map((record: (String, Seq[Range])) =>
      record match {
        case (id, naps) =>
          val mostNapsOneMinute: (Int, Int) = numberAppearingMostWithCount(naps)
          (id, mostNapsOneMinute._1, mostNapsOneMinute._2)
    })
  }

  def mostSleepingSameMinute(records: Map[String, Seq[Range]]): (String, Int, Int) = {
    recordsToMostNapsOneMinute(records).maxBy(_._3)
  }
}
