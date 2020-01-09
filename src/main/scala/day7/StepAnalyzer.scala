package day7

import scala.util.matching.Regex

object StepAnalyzer {

  def stringsToSteps(strings: Seq[String]): Set[(Char, Char)] = {
    val reg: Regex = raw"Step ([A-Z]) must be finished before step ([A-Z]) can begin\.".r("prereq", "step")
    (for (s <- strings) yield {
      val mi: Regex.MatchIterator = reg.findAllIn(s)
      (mi.group("prereq").head, mi.group("step").head)
    }).toSet
  }

  def stepsToOrder(steps: Set[(Char, Char)]): String = {
    val allSteps: Set[Char] = steps.map(_._1) ++ steps.map(_._2)
    val stepsWithPrereqs: Map[Char, Set[Char]] = steps.groupMap(_._2)(_._1)
    val stepsWithNoPrereqs: Set[Char] = allSteps.filterNot(stepsWithPrereqs.contains)
    var nextSteps: Seq[Char] = stepsWithNoPrereqs.toSeq.sorted
    var executedSteps: Seq[Char] = Seq.empty
    while (nextSteps.nonEmpty) {
      executedSteps = executedSteps :+ nextSteps.head
      val availableSteps: Set[Char] = stepsWithNoPrereqs ++ stepsWithPrereqs
        .filter(_._2.subsetOf(executedSteps.toSet))
        .keySet
      nextSteps = availableSteps.diff(executedSteps.toSet).toSeq.sorted
    }
    executedSteps.mkString
  }

  def stepsToOrder2(steps: Set[(Char, Char)], workerCount: Int, duration: Char => Int): (String, Int) = {
    val allSteps: Set[Char] = steps.map(_._1) ++ steps.map(_._2)
    val stepsWithPrereqs: Map[Char, Set[Char]] = steps.groupMap(_._2)(_._1)
    val stepsWithNoPrereqs: Set[Char] = allSteps.filterNot(stepsWithPrereqs.contains)
    var workingSteps: Set[(Char, Int)] =
      stepsWithNoPrereqs.toSeq.sorted.take(workerCount).toSet.map((c: Char) => (c, duration(c)))
    var completedSteps: Seq[Char] = Seq.empty
    var seconds = 0
    while (workingSteps.nonEmpty) {
      workingSteps = workingSteps.map((s: (Char, Int)) => (s._1, s._2 - 1))
      completedSteps = completedSteps ++ workingSteps.filter(_._2 < 1).map(_._1).toSeq.sorted
      workingSteps = workingSteps.filterNot(_._2 < 1)
      val availableSteps: Set[Char] =
        (stepsWithNoPrereqs ++ stepsWithPrereqs.filter(_._2.subsetOf(completedSteps.toSet)).keySet)
          .diff(completedSteps.toSet ++ workingSteps.map(_._1))
      val newWorkingSteps: Set[(Char, Int)] =
        availableSteps.toSeq.sorted.take(workerCount - workingSteps.size).map((c: Char) => (c, duration(c))).toSet
      workingSteps = workingSteps ++ newWorkingSteps
      seconds += 1
    }
    (completedSteps.mkString, seconds)
  }
}
