package day1

object Device {
  def resultingFrequency(changes: Seq[Int]): Int = {
    changes.sum
  }

  def firstFreqReachedTwice(changes: Seq[Int]): Int = {
    var curFreq: Int = 0
    var reachedFreqs: Set[Int] = Set(curFreq)
    var reachedTwice: Option[Integer] = None

    while (reachedTwice.isEmpty) {
      changes.foreach((i: Int) => {
        curFreq += i
        if (reachedFreqs.contains(curFreq) && reachedTwice.isEmpty) {
          reachedTwice = Some(curFreq)
        } else {
          reachedFreqs += curFreq
        }
      })
    }
    reachedTwice.get
  }
}
