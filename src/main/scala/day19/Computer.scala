package day19

class Computer(var r: Registers) {
  val opCodeMap: Map[String, Operation] =
    Set(ADDI, ADDR, BANI, BANR, BONI, BONR, EQIR, EQRI, EQRR, GTIR, GTRI, GTRR, MULI, MULR, SETI, SETR).map((op: Operation) =>
      (op.getClass.getSimpleName.dropRight(1).toLowerCase, op)).toMap
  var ip: Long = 0

  def execute(instructions: Seq[Instruction], debug: Registers => Unit = (_: Registers) => {}): Registers = {
    var prev0: Long = 0
    var ipOffset: Short = 0
    var ipRegID: Option[Short] = None
    while (instructions.indices.contains(ip + ipOffset)) {
      instructions((ip + ipOffset).toInt) match {
        case op: OpInstruction =>
          if (ipRegID.isDefined) r = r.updateByNum(ipRegID.get, ip)
          r = opCodeMap(op.opCode).apply(r, op)
          if (prev0 != r(0)) {
            debug(r)
            prev0 = r(0)
          }
          if (ipRegID.isDefined) ip = r(ipRegID.get)
          ip = (ip + 1).toShort
        case ipInst: IPInstruction =>
          ipRegID = Some(ipInst.regID)
          ipOffset = (ipOffset + 1).toShort
      }
    }
    r
  }
}

object Computer {
  def stringToInstruction(s: String): Instruction = {
    if (s.startsWith("#ip ")) IPInstruction(s.split(' ')(1).toShort)
    else {
      val inst: Seq[String] = s.split(' ').toSeq
      OpInstruction(inst.head, inst(1).toShort, inst(2).toShort, inst(3).toShort)
    }
  }

  def stringsToInstructions(strings: Seq[String]): Seq[Instruction] = {
    if (strings.isEmpty || strings.head.isEmpty) Seq.empty
    else stringToInstruction(strings.head) +: stringsToInstructions(strings.tail)
  }

}

trait Operation {
  val op: (Registers, OpInstruction) => Long

  def apply(before: Registers, inst: OpInstruction): Registers = {
    before.updateByNum(inst.outputC, op(before, inst))
  }
}

object ADDR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) + before(inst.inputB)
}

object ADDI extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) + inst.inputB
}

object MULR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) * before(inst.inputB)
}

object MULI extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) * inst.inputB
}

object BANR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) & before(inst.inputB)
}

object BANI extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) & inst.inputB
}

object BONR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) | before(inst.inputB)
}

object BONI extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) | inst.inputB
}

object SETR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA)
}

object SETI extends Operation {
  override val op: (Registers, OpInstruction) => Long = (_: Registers, inst: OpInstruction) => inst.inputA
}

object GTIR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => if (inst.inputA > before(inst.inputB)) 1 else 0
}

object GTRI extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => if (before(inst.inputA) > inst.inputB) 1 else 0
}

object GTRR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => if (before(inst.inputA) > before(inst.inputB)) 1 else 0
}

object EQIR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => if (inst.inputA == before(inst.inputB)) 1 else 0
}

object EQRI extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => if (before(inst.inputA) == inst.inputB) 1 else 0
}

object EQRR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => if (before(inst.inputA) == before(inst.inputB)) 1 else 0
}


case class Registers(_0: Long, _1: Long, _2: Long, _3: Long, _4: Long, _5: Long) {
  def apply(regNum: Short): Long = {
    regNum match {
      case 0 => _0
      case 1 => _1
      case 2 => _2
      case 3 => _3
      case 4 => _4
      case 5 => _5
    }
  }

  def updateByNum(regNum: Short, newValue: Long): Registers = {
    regNum match {
      case 0 => copy(_0 = newValue)
      case 1 => copy(_1 = newValue)
      case 2 => copy(_2 = newValue)
      case 3 => copy(_3 = newValue)
      case 4 => copy(_4 = newValue)
      case 5 => copy(_5 = newValue)
    }
  }
}

trait Instruction

case class OpInstruction(opCode: String, inputA: Short, inputB: Short, outputC: Short) extends Instruction

case class IPInstruction(regID: Short) extends Instruction
