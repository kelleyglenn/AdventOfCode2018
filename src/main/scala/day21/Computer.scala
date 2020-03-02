package day21

class Computer(var r: Registers) {
  var ip: Long = 0

  def execute(instructions: Seq[Instruction], debugAndExit: Registers => Boolean = (_: Registers) => {
    false
  }): (Registers, Int) = {
    var instCt: Int = 0
    var ipOffset: Short = 0
    var ipRegID: Option[Short] = None
    var exit = false
    while (instructions.indices.contains(ip + ipOffset) && !exit) {
      instructions((ip + ipOffset).toInt) match {
        case op: OpInstruction =>
          if (ipRegID.isDefined) r = r.updateByNum(ipRegID.get, ip)
          r = Computer.opCodeMap(op.opCode).apply(r, op)
          exit = debugAndExit(r)
          instCt += 1
          if (ipRegID.isDefined) ip = r(ipRegID.get)
          ip = (ip + 1).toShort
        case ipInst: IPInstruction =>
          ipRegID = Some(ipInst.regID)
          ipOffset = (ipOffset + 1).toShort
      }
    }
    (r, instCt)
  }
}

object Computer {
  val opCodeMap: Map[String, Operation] =
    Set(ADDI, ADDR, BANI, BANR, BORI, BORR, EQIR, EQRI, EQRR, GTIR, GTRI, GTRR, MULI, MULR, SETI, SETR).map((op: Operation) =>
      (op.getClass.getSimpleName.dropRight(1).toLowerCase, op)).toMap

  def stringToInstruction(s: String): Instruction = {
    if (s.startsWith("#ip ")) IPInstruction(s.split(' ')(1).toShort)
    else {
      val inst: Seq[String] = s.split(' ').toSeq
      OpInstruction(inst.head, inst(1).toInt, inst(2).toInt, inst(3).toInt)
    }
  }

  def stringsToInstructions(strings: Seq[String]): Seq[Instruction] = {
    if (strings.isEmpty || strings.head.isEmpty) Seq.empty
    else stringToInstruction(strings.head) +: stringsToInstructions(strings.tail)
  }

  def instructionsToMkStrings(instructions: Seq[Instruction]): Seq[String] = {
    instructions.map(_.mkString)
  }
}

trait Operation {
  val op: (Registers, OpInstruction) => Long

  def apply(before: Registers, inst: OpInstruction): Registers = {
    before.updateByNum(inst.outputC, op(before, inst))
  }

  def mkStringSuffix(inst: OpInstruction): String

}

object ADDR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) + before(inst.inputB)

  override def mkStringSuffix(inst: OpInstruction): String = "Reg(" + inst.inputA + ") + Reg(" + inst.inputB + ")"
}

object ADDI extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) + inst.inputB

  override def mkStringSuffix(inst: OpInstruction): String = "Reg(" + inst.inputA + ") + " + inst.inputB
}

object MULR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) * before(inst.inputB)

  override def mkStringSuffix(inst: OpInstruction): String = "Reg(" + inst.inputA + ") * Reg(" + inst.inputB + ")"
}

object MULI extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) * inst.inputB

  override def mkStringSuffix(inst: OpInstruction): String = "Reg(" + inst.inputA + ") * " + inst.inputB
}

object BANR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) & before(inst.inputB)

  override def mkStringSuffix(inst: OpInstruction): String = "Reg(" + inst.inputA + ") & Reg(" + inst.inputB + ")"
}

object BANI extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) & inst.inputB

  override def mkStringSuffix(inst: OpInstruction): String = "Reg(" + inst.inputA + ") & " + inst.inputB
}

object BORR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) | before(inst.inputB)

  override def mkStringSuffix(inst: OpInstruction): String = "Reg(" + inst.inputA + ") | Reg(" + inst.inputB + ")"
}

object BORI extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA) | inst.inputB

  override def mkStringSuffix(inst: OpInstruction): String = "Reg(" + inst.inputA + ") | " + inst.inputB
}

object SETR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => before(inst.inputA)

  override def mkStringSuffix(inst: OpInstruction): String = "Reg(" + inst.inputA + ")"
}

object SETI extends Operation {
  override val op: (Registers, OpInstruction) => Long = (_: Registers, inst: OpInstruction) => inst.inputA

  override def mkStringSuffix(inst: OpInstruction): String = inst.inputA.toString
}

object GTIR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => if (inst.inputA > before(inst.inputB)) 1 else 0

  override def mkStringSuffix(inst: OpInstruction): String = inst.inputA.toString + " > Reg(" + inst.inputB + ") ? 1 : 0"
}

object GTRI extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => if (before(inst.inputA) > inst.inputB) 1 else 0

  override def mkStringSuffix(inst: OpInstruction): String = "Reg(" + inst.inputA + ") > " + inst.inputB + " ? 1 : 0"
}

object GTRR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => if (before(inst.inputA) > before(inst.inputB)) 1 else 0

  override def mkStringSuffix(inst: OpInstruction): String = "Reg(" + inst.inputA + ") > Reg(" + inst.inputB + ") ? 1 : 0"
}

object EQIR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => if (inst.inputA == before(inst.inputB)) 1 else 0

  override def mkStringSuffix(inst: OpInstruction): String = inst.inputA.toString + " == Reg(" + inst.inputB + ") ? 1 : 0"
}

object EQRI extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => if (before(inst.inputA) == inst.inputB) 1 else 0

  override def mkStringSuffix(inst: OpInstruction): String = "Reg(" + inst.inputA + ") == " + inst.inputB + " ? 1 : 0"
}

object EQRR extends Operation {
  override val op: (Registers, OpInstruction) => Long = (before: Registers, inst: OpInstruction) => if (before(inst.inputA) == before(inst.inputB)) 1 else 0

  override def mkStringSuffix(inst: OpInstruction): String = "Reg(" + inst.inputA + ") == Reg(" + inst.inputB + ") ? 1 : 0"
}


case class Registers(_0: Long, _1: Long, _2: Long, _3: Long, _4: Long, _5: Long) {
  def apply(regNum: Int): Long = {
    regNum match {
      case 0 => _0
      case 1 => _1
      case 2 => _2
      case 3 => _3
      case 4 => _4
      case 5 => _5
    }
  }

  def updateByNum(regNum: Int, newValue: Long): Registers = {
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

trait Instruction {
  def mkString: String
}

case class OpInstruction(opCode: String, inputA: Int, inputB: Int, outputC: Int) extends Instruction {
  override def mkString: String = {
    opCode + ": Set Reg(" + outputC + ") to " + Computer.opCodeMap(opCode).mkStringSuffix(this)
  }
}

case class IPInstruction(regID: Short) extends Instruction {
  override def mkString: String = "Tie IP to Reg(" + regID + ")"
}

class SimulatedInput(var r: Registers) {
  def execute(debugAndExit: Registers => Boolean = (_: Registers) => {
    false
  }): Registers = {
    var exit: Boolean = line6(debugAndExit)
    while (r._0 != r._2 && !exit) {
      exit = debugAndExit(r.copy(_4 = 28))
      if (!exit) exit = line6(debugAndExit)
    }
    debugAndExit(r.copy(_4 = 28))
    r
  }

  def line6(debugAndExit: Registers => Boolean): Boolean = {
    var exit: Boolean = debugAndExit(r.copy(_4 = 5))
    r = r.copy(_5 = r._2 | 65536, _2 = 5234604)
    if (!exit) {
      exit = line8(debugAndExit)
      while (r._3 != 1 && !exit) {
        if (!exit) {
          exit = line18(debugAndExit)
          if (r._1 == 1 && !exit) {
            r = r.copy(_5 = r._3)
            if (!exit) exit = line8(debugAndExit)
          }
        }
      }
    }
    exit
  }

  def line8(debugAndExit: Registers => Boolean): Boolean = {
    // line 8,9,10,11,12,13
    var exit: Boolean = debugAndExit(r.copy(_4 = 7))
    if (!exit) {
      val r2: Long = (((r._2 + (r._5 % 256)) % 16777216) * 65899) % 16777216
      r = r.copy(_2 = r2, _3 = if (256 > r._5) 0 else r._3)
      exit = debugAndExit(r.copy(_4 = 13))
    }
    exit
  }

  def line18(debugAndExit: Registers => Boolean): Boolean = {
    // line 18,19
    var exit: Boolean = debugAndExit(r.copy(_4 = 17))
    if (!exit) {
      r = r.copy(_1 = if ((r._3 + 1) * 256 > r._5) 1 else 0)
      exit = debugAndExit(r.copy(_4 = 20))
    }
    exit
  }
}

class SimulatedInput2(targetValue: Long) {
  def execute(debug: Registers => Boolean = (_: Registers) => {
    false
  }): Unit = {
    var curR: Registers = nextValue(Registers(targetValue, 0, 0, 0, 0, 0))
    var exit: Boolean = debug(curR.copy(_4 = 27))
    while (curR._0 != curR._2 && !exit) {
      curR = nextValue(curR)
      exit = debug(curR.copy(_4 = 27))
    }
  }

  def nextValue(r: Registers): Registers = {
    var newR: Registers = line6to12(r)
    if (255 > newR._5) newR
    else {
      newR = line18to19(newR.copy(_3 = 0))
      while (newR._1 <= newR._5) {
        newR = line18to19(r.copy(_3 = newR._3 + 1))
      }
      newR.copy(_5 = newR._3)
    }
  }

  def line6to12(r: Registers): Registers = {
    line8to12(r.copy(_2 = 5234604, _5 = r._2 | 65536))
  }

  def line8to12(r: Registers): Registers = {
    val reg2: Long = ((((r._5 % 256) + r._2) % (256 * 256 * 256)) * ((256 * 256) + 363)) % (256 * 256 * 256)
    r.copy(_2 = reg2, _3 = r._5 % 256)
  }

  def line18to19(r: Registers): Registers = {
    r.copy(_1 = (r._3 + 1) * 256)
  }
}
