package day16

object OperationMatcher {

  case class Registers(_0: Short, _1: Short, _2: Short, _3: Short) {
    def apply(regNum: Short): Short = {
      regNum match {
        case 0 => _0
        case 1 => _1
        case 2 => _2
        case 3 => _3
      }
    }

    def updateByNum(regNum: Short, newValue: Short): Registers = {
      regNum match {
        case 0 => copy(_0 = newValue)
        case 1 => copy(_1 = newValue)
        case 2 => copy(_2 = newValue)
        case 3 => copy(_3 = newValue)
      }
    }
  }

  case class Instruction(opCode: Short, inputA: Short, inputB: Short, outputC: Short)

  case class Sample(before: Registers, inst: Instruction, after: Registers)

  trait Operation {
    val op: (Registers, Instruction) => Int

    def canBe(before: Registers, inst: Instruction, after: Registers): Boolean = {
      after(inst.outputC) == op(before, inst) && registersUnchanged(before, after, inst.outputC)
    }

    def apply(before: Registers, inst: Instruction): Registers = {
      before.updateByNum(inst.outputC, op(before, inst).toShort)
    }
  }

  def operationsThatMatch(s: Sample): Set[Operation] = {
    operations.filter(_.canBe(s.before, s.inst, s.after))
  }

  def samplesThatMatchOperations(samples: Set[Sample],
                                 minMatches: Int): Set[Sample] = {
    samples.filter((s: Sample) => operationsThatMatch(s).size >= minMatches)
  }

  def opCodesToOperations(
                           samples: Set[Sample]
                         ): Option[Map[Short, Operation]] = {
    var possibleOpCodesToOperations: Map[Short, Set[Operation]] =
      samples.foldLeft(Map[Short, Set[Operation]]().empty.withDefaultValue(Set.empty)) {
        case (m, s) => m.updated(s.inst.opCode, m(s.inst.opCode) ++ operationsThatMatch(s))
      }
    var possibleOperationsToOpCodes: Map[Operation, Set[Short]] =
      possibleOpCodesToOperations.foldLeft(Map[Operation, Set[Short]]().withDefaultValue(Set.empty)) {
        case (m, (s, ops)) => ops.foldLeft(m) { case (m1, op) => m1.updated(op, m(op) + s) }
      }
    val allOpCodes: Set[Short] = possibleOpCodesToOperations.keySet
    var knownOpCodesToOperations: Map[Short, Operation] = Map.empty
    while (possibleOpCodesToOperations.exists(_._2.size == 1) || possibleOperationsToOpCodes
      .exists(_._2.size == 1)) {
      val known: (Short, Operation) =
        if (possibleOpCodesToOperations.exists(_._2.size == 1))
          possibleOpCodesToOperations.find(_._2.size == 1).get match {
            case (code, ops) => (code, ops.head)
          } else
          possibleOperationsToOpCodes.find(_._2.size == 1).get match {
            case (ops, code) => (code.head, ops)
          }
      knownOpCodesToOperations = knownOpCodesToOperations + known
      possibleOpCodesToOperations =
        possibleOpCodesToOperations.removed(known._1)
      possibleOpCodesToOperations = possibleOpCodesToOperations.map(
        (co: (Short, Set[Operation])) => (co._1, co._2 - known._2)
      )
      possibleOperationsToOpCodes =
        possibleOperationsToOpCodes.removed(known._2)
      possibleOperationsToOpCodes = possibleOperationsToOpCodes.map(
        (oc: (Operation, Set[Short])) => (oc._1, oc._2 - known._1)
      )
    }
    if (knownOpCodesToOperations.size == allOpCodes.size)
      Some(knownOpCodesToOperations)
    else None
  }

  def runCode(instructions: Seq[Instruction], codedOperations: Map[Short, Operation], input: Registers): Registers = {
    var registers: Registers = input
    instructions.foreach { inst: Instruction =>
      registers = codedOperations(inst.opCode)(registers, inst)
    }
    registers
  }

  def stringsToSample(strings: Seq[String]): Sample = {
    val s: Seq[String] = strings.take(3)
    val before: Seq[Short] =
      s.head.split(": ").last.tail.dropRight(1).split(", ").map(_.toShort).toSeq
    val after: Seq[Short] = s.last.split(raw" {2}").last.tail.dropRight(1).split(", ").map(_.toShort).toSeq
    Sample(
      Registers(before.head, before(1), before(2), before(3)),
      stringToInstruction(s(1)),
      Registers(after.head, after(1), after(2), after(3))
    )
  }

  def stringsToSamples(strings: Seq[String]): Set[Sample] = {
    if (strings.size > 3) stringsToSamples(strings.drop(4)) + stringsToSample(strings) else Set.empty
  }

  def stringToInstruction(s: String): Instruction = {
    val inst: Seq[Short] = s.split(" ").map(_.toShort).toSeq
    Instruction(inst.head, inst(1), inst(2), inst(3))
  }

  def stringsToInstructions(strings: Seq[String]): Seq[Instruction] = {
    if (strings.isEmpty || strings.head.isEmpty) Seq.empty
    else stringToInstruction(strings.head) +: stringsToInstructions(strings.tail)
  }

  val operations: Set[Operation] =
    Set(
      ADDI,
      ADDR,
      BANI,
      BANR,
      BONI,
      BONR,
      EQIR,
      EQRI,
      EQRR,
      GTIR,
      GTRI,
      GTRR,
      MULI,
      MULR,
      SETI,
      SETR
    )

  def registersUnchanged(before: Registers,
                         after: Registers,
                         except: Short): Boolean = {
    val unchanged: Set[Short] = Set[Short](0, 1, 2, 3) - except
    !unchanged.exists((i: Short) => after(i) != before(i))
  }

  object ADDR extends Operation {
    override val op: (Registers, Instruction) => Int = (before: Registers, inst: Instruction) => before(inst.inputA) + before(inst.inputB)
  }

  object ADDI extends Operation {
    override val op: (Registers, Instruction) => Int = (before: Registers, inst: Instruction) => before(inst.inputA) + inst.inputB
  }

  object MULR extends Operation {
    override val op: (Registers, Instruction) => Int = (before: Registers, inst: Instruction) => before(inst.inputA) * before(inst.inputB)
  }

  object MULI extends Operation {
    override val op: (Registers, Instruction) => Int = (before: Registers, inst: Instruction) => before(inst.inputA) * inst.inputB
  }

  object BANR extends Operation {
    override val op: (Registers, Instruction) => Int = (before: Registers, inst: Instruction) => before(inst.inputA) & before(inst.inputB)
  }

  object BANI extends Operation {
    override val op: (Registers, Instruction) => Int = (before: Registers, inst: Instruction) => before(inst.inputA) & inst.inputB
  }

  object BONR extends Operation {
    override val op: (Registers, Instruction) => Int = (before: Registers, inst: Instruction) => before(inst.inputA) | before(inst.inputB)
  }

  object BONI extends Operation {
    override val op: (Registers, Instruction) => Int = (before: Registers, inst: Instruction) => before(inst.inputA) | inst.inputB
  }

  object SETR extends Operation {
    override val op: (Registers, Instruction) => Int = (before: Registers, inst: Instruction) => before(inst.inputA)
  }

  object SETI extends Operation {
    override val op: (Registers, Instruction) => Int = (_: Registers, inst: Instruction) => inst.inputA
  }

  object GTIR extends Operation {
    override val op: (Registers, Instruction) => Int = (before: Registers, inst: Instruction) => if (inst.inputA > before(inst.inputB)) 1 else 0
  }

  object GTRI extends Operation {
    override val op: (Registers, Instruction) => Int = (before: Registers, inst: Instruction) => if (before(inst.inputA) > inst.inputB) 1 else 0
  }

  object GTRR extends Operation {
    override val op: (Registers, Instruction) => Int = (before: Registers, inst: Instruction) => if (before(inst.inputA) > before(inst.inputB)) 1 else 0
  }

  object EQIR extends Operation {
    override val op: (Registers, Instruction) => Int = (before: Registers, inst: Instruction) => if (inst.inputA == before(inst.inputB)) 1 else 0
  }

  object EQRI extends Operation {
    override val op: (Registers, Instruction) => Int = (before: Registers, inst: Instruction) => if (before(inst.inputA) == inst.inputB) 1 else 0
  }

  object EQRR extends Operation {
    override val op: (Registers, Instruction) => Int = (before: Registers, inst: Instruction) => if (before(inst.inputA) == before(inst.inputB)) 1 else 0
  }

}
