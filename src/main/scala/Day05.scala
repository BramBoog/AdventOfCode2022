object Day05 extends App:
    import scala.io.Source
    import scala.util.matching.Regex

    val input: Array[Array[String]] =
        Source.fromResource("InputDay05.txt").mkString.split("\r\n\r\n").map(_.split("\r\n"))

    val stacks: Map[Int, List[Char]] =
        val startConf: List[String] = input.head.toList
        val stackNumbers: List[Int] =
            val keyPattern = "[0-9]+".r
            keyPattern
            .findAllMatchIn(startConf.last)
            .map(_.matched.toInt)
            .toList
        val stacksToParse: List[String] = startConf.dropRight(1)
        (for
            stack <- stackNumbers
        yield
            (stack,
                for 
                    line <- stacksToParse if !line((stack-1)*4+1).isWhitespace
                yield line((stack-1)*4+1)
            )
        ).toMap
        
    case class Instruction(amount: Int, from: Int, to: Int):
        def execute(stackMap: Map[Int, List[Char]], reverse: Boolean) =
            def getStack(stackMap: Map[Int, List[Char]], stack: Int): List[Char] =
            stackMap.get(stack) match
                case None => sys.error(s"Couldn't find stack #${stack}.")
                case Some(l) => l

            val fromStack = getStack(stackMap, from)
            val toStack = getStack(stackMap, to)
            val toMove =
                if (reverse) fromStack.take(amount).reverse
                else fromStack.take(amount)
            stackMap + (from -> fromStack.drop(amount)) + (to -> toMove.concat(toStack))

    def parseInstruction(s: String) =
        s match
            case s"move ${a} from ${f} to ${t}" => Instruction(a.toInt, f.toInt, t.toInt)
            case _ => sys.error(s"Undefined input: ${s}.")
        
    val instructions: List[Instruction] =
        input
        .last
        .map(parseInstruction)
        .toList
    
    def executeInstructions(instr: List[Instruction], stackMap: Map[Int, List[Char]], reverse: Boolean = true): String =
        instr.headOption match
            case None => stackMap.toList.sortBy((s, l) => s).map((s, l) => l.head).mkString
            case Some(i) => executeInstructions(instr.tail, i.execute(stackMap, reverse), reverse)
        
    val answer1: String = executeInstructions(instructions, stacks)
    println(s"Result part 1: ${answer1}")

    val answer2: String = executeInstructions(instructions, stacks, false)
    println(s"Result part 2: ${answer2}")
