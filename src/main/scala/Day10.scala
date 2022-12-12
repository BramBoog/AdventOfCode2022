object Day10 extends App:
    import scala.io.Source

    val input: List[Instruction] =
        Source.fromResource("InputDay10.txt").getLines.toList
        .flatMap({
            case "noop" => List(NoOp)
            case s"addx ${v}" => List(NoOp, AddX(v.toInt))
        })

    sealed trait Instruction
    
    object NoOp extends Instruction

    case class AddX(value: Int) extends Instruction

    case class CPU(regisX: Int, cycle: Int):
        def signalStrength: Int = regisX * cycle
        def increment: CPU = this.copy(cycle = cycle + 1)
        def addX(v: Int): CPU = this.copy(regisX = regisX + v)

    def followInstructionsToAccumulator[A](instrList: List[Instruction], cpu: CPU, acc: A, addToAcc: (CPU, A) => A, cutoff: Int): A =
        val incrementedCPU = cpu.increment
        val newAcc = addToAcc(incrementedCPU, acc)

        if (incrementedCPU.cycle == cutoff) newAcc
        else if (instrList.isEmpty) sys.error("Did not reach the desired amount of cycles given the instructions.")
        else instrList.head match
            case NoOp => followInstructionsToAccumulator(instrList.tail, incrementedCPU, newAcc, addToAcc, cutoff)
            case AddX(v) => followInstructionsToAccumulator(instrList.tail, incrementedCPU.addX(v), newAcc, addToAcc, cutoff)

    val initCPU: CPU = CPU(1, 0)

    // Part 1

    def addSignalStrength(cpu: CPU, signalStrengths: List[Int]): List[Int] =
        if (cpu.cycle == 20 | cpu.cycle % 40 == 20)
            signalStrengths :+ cpu.signalStrength
        else signalStrengths

    val answer1: Int = followInstructionsToAccumulator(input, initCPU, List(), addSignalStrength, 220).sum
    println(s"Result part 1: ${answer1}")

    // Part 2

    def addDrawnPixel(cpu: CPU, pixels: String): String =
        val pixelPos = (cpu.cycle - 1) % 40
        if (pixelPos >= cpu.regisX - 1 & pixelPos <= cpu.regisX + 1) pixels + "#"
        else pixels + "."

    println("Result part 2:")
    followInstructionsToAccumulator(input, initCPU, "", addDrawnPixel, 240).grouped(40).foreach(println)
