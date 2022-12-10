object Day09 extends App:
    import scala.io.Source
    import scala.math
    
    case class Instruction(direction: Char, steps: Int)

    val input: List[Instruction] =
        Source.fromResource("InputDay09.txt").getLines.toList
        .map({case s"${d} ${s}" => Instruction(d.head, s.toInt)})

    case class Position(x: Int, y: Int)

    sealed trait Knot:
        def pos: Position

        def follow(lead: Position): Position =
            val delta = Position(lead.x - pos.x, lead.y - pos.y)
            if (math.abs(delta.x) == 2 | math.abs(delta.y) == 2) Position(pos.x + delta.x.sign, pos.y + delta.y.sign)
            else pos

    case class Head(pos: Position) extends Knot:
        def takeInstruction(instr: Instruction): Position =
            instr.direction match
                case 'U' => Position(pos.x, pos.y+1)
                case 'D' => Position(pos.x, pos.y-1)
                case 'R' => Position(pos.x+1, pos.y)
                case 'L' => Position(pos.x-1, pos.y)

    case class Tail(pos: Position, visited: Set[Position]) extends Knot:
        def followWithVisited(lead: Position): Tail =
            val newPos = follow(lead)
            Tail(newPos, visited.incl(newPos))

    case class MiddleKnot(pos: Position) extends Knot

    def evaluateInstructions(instr: List[Instruction], h: Head, t: Tail, middleKnots: Map[Int, MiddleKnot] = Map()): Set[Position] =
        if (instr.isEmpty) t.visited
        else 
            val remInstr =
                if (instr.head.steps == 1) instr.tail
                else List(instr.head.copy(steps = instr.head.steps - 1)) ::: instr.tail
            val newHeadPos = h.takeInstruction(instr.head)

            if (middleKnots.isEmpty) evaluateInstructions(remInstr, Head(newHeadPos), t.followWithVisited(newHeadPos))
            else
                def moveKnots(knotsMap: Map[Int, MiddleKnot], n: Int = 2): Map[Int, MiddleKnot] =
                knotsMap.get(n) match
                    case None => knotsMap
                    case Some(k) =>
                        moveKnots(knotsMap + (n -> MiddleKnot(k.follow(knotsMap.get(n-1).get.pos))), n+1)

                val newKnots = moveKnots(middleKnots + (1 -> MiddleKnot(middleKnots.get(1).get.follow(newHeadPos))))
                evaluateInstructions(remInstr, Head(newHeadPos), t.followWithVisited(newKnots.get(newKnots.map(_._1).max).get.pos), newKnots)
            

    val answer1: Int = evaluateInstructions(input, Head(Position(0,0)), Tail(Position(0,0), Set(Position(0,0)))).size
    println(s"Result part 1: ${answer1}")

    val middleKnotsInit =
        (
        for i <- Range(1, 9)
        yield (i, MiddleKnot(Position(0,0)))
        ).toMap

    val answer2: Int = evaluateInstructions(input, Head(Position(0,0)), Tail(Position(0,0), Set(Position(0,0))), middleKnotsInit).size
    println(s"Result part 2: ${answer2}")
