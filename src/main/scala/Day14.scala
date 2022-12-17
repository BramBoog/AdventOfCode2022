object Day14 extends App:
    import scala.io.Source

    case class Pos(x: Int, y: Int)

    def createRockCoordinates(start: Array[Int], end: Array[Int]): List[Pos] =
        if (start(0) == end(0))
            val line =
                if (start(1) < end(1)) Range(start(1), end(1) + 1).toList
                else Range(start(1), end(1) - 1, -1).toList
            line.zip(List.fill(line.length)(start(0))).map((y,x) => Pos(x,y))
        else if (start(1) == end(1))
            val line = 
                if (start(0) < end(0)) Range(start(0), end(0) + 1).toList
                else Range(start(0), end(0) - 1, -1).toList
            line.zip(List.fill(line.length)(start(1))).map((x,y) => Pos(x,y))
        else sys.error(s"Start: ${start} and end: ${end} have no coordinate in common")

    val rockCoordinates: Set[Pos] =
        Source.fromResource("InputDay14.txt").getLines.toList
        .map(_
            .split(" -> ").map(_.split(",").map(_.toInt))
            .sliding(2)
            .map(a => createRockCoordinates(a(0), a(1)))
        ).flatten.flatten.toSet

    val lowestRock: Int = rockCoordinates.map(_.y).max
    val entryPoint: Pos = Pos(500, 0)

    case class SandUnit(pos: Pos):
        def possibleMove(filledPos: Set[Pos], floor: Option[Int]): Option[Pos] =
            if (floor.isDefined && pos.y == floor.get - 1) None
            else List(pos.copy(y = pos.y + 1), Pos(pos.x - 1, pos.y + 1), Pos(pos.x + 1, pos.y + 1)).find(p => !filledPos.contains(p))

    // Part 1

    def dropSandUntilAbyss(filledPos: Set[Pos], curUnit: SandUnit = SandUnit(entryPoint)): Set[Pos] =
        val move = curUnit.possibleMove(filledPos, None)
        move match
            case None => dropSandUntilAbyss(filledPos.incl(curUnit.pos))
            case Some(Pos(x, y)) if y == lowestRock => filledPos
            case Some(pos) => dropSandUntilAbyss(filledPos, SandUnit(pos))
    
    val answer1: Int = dropSandUntilAbyss(rockCoordinates).size - rockCoordinates.size
    println(s"Result part 1: ${answer1}")

    // Part 2

    val floorLevel: Int = lowestRock + 2

    def dropSandUntilPlugged(filledPos: Set[Pos], curUnit: SandUnit = SandUnit(entryPoint)): Set[Pos] =
        val move = curUnit.possibleMove(filledPos, Some(floorLevel))
        move match
            case None if curUnit.pos == entryPoint => filledPos.incl(entryPoint)
            case None => dropSandUntilPlugged(filledPos.incl(curUnit.pos))
            case Some(pos) => dropSandUntilPlugged(filledPos, SandUnit(pos))

    val answer2: Int = dropSandUntilPlugged(rockCoordinates).size - rockCoordinates.size
    println(s"Result part 2: ${answer2}")
