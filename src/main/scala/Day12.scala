object Day12 extends App:
    import scala.io.Source
    import scala.math

    val input: List[List[Char]] =
        Source.fromResource("InputDay12.txt")
        .getLines
        .toList
        .map(_.toList)

    case class Loc(x: Int, y: Int)

    val startLoc =
        val startY = input.indexWhere(_.contains('S'))
        Loc(input(startY).indexOf('S'), startY)

    val endLoc =
        val endY = input.indexWhere(_.contains('E'))
        Loc(input(endY).indexOf('E'), endY)

    sealed trait Position:
        def loc: Loc
        def z: Char
        def explored: Boolean

        def possibleMoves(posMap: Map[Loc, Position]): List[Position] =
            for
                deltaX <- List(-1, 0, 1)
                deltaY <- List(-1, 0, 1)
                if math.abs(deltaX) + math.abs(deltaY) == 1
                    && posMap.get(Loc(loc.x + deltaX, loc.y + deltaY)).isDefined
                    && !posMap.get(Loc(loc.x + deltaX, loc.y + deltaY)).get.explored
                    && posMap.get(Loc(loc.x + deltaX, loc.y + deltaY)).get.z <= z + 1
            yield
                posMap.get(Loc(loc.x + deltaX, loc.y + deltaY)).get

    case class Pos(loc: Loc, z: Char, explored: Boolean) extends Position

    object End extends Position:
        val loc: Loc = endLoc
        val z: Char = 'z'
        val explored: Boolean = false

    case class Start(loc: Loc) extends Position:
        val z: Char = 'a'
        val explored: Boolean = true

    case class Path(positions: List[Position]):
        def length: Int = positions.length

    def findPath(grid: Map[Loc, Position], pathsToExtend: List[Path], extendedPaths: List[Path] = List()): Option[Path] =
        // Using breadth-first algorithm
        val newPaths =
            val positions = pathsToExtend.head.positions
            for next <- positions.head.possibleMoves(grid)
            yield Path(List(next) ::: positions)
        if (newPaths.isEmpty && pathsToExtend.length == 1 && extendedPaths.isEmpty) None // All possible paths from the starting position have reached a dead end
        else if (newPaths.map(_.positions.head).contains(End)) Some(newPaths.filter(_.positions.head == End).head)
        else
            val newGrid = newPaths.map(_.positions.head).foldLeft(grid)((g, p) => g + (p.loc -> Pos(p.loc, p.z, true)))
            if (pathsToExtend.length == 1) findPath(newGrid, extendedPaths ::: newPaths)
            else findPath(newGrid, pathsToExtend.tail, extendedPaths ::: newPaths)

    val gridInit: Map[Loc, Position] =
        input
        .map(_.zipWithIndex)
        .zipWithIndex
        .map((l, y) => l.map((c, x) => (Loc(x,y), Pos(Loc(x,y), c, false))))
        .flatten.toMap
        + (End.loc -> End)

    // Part 1

    val gridInitPart1: Map[Loc, Position] = gridInit + (startLoc -> Start(startLoc))

    val answer1: Int = findPath(gridInitPart1, List(Path(List(Start(startLoc))))).get.length - 1
    println(s"Result part 1: ${answer1}")

    // Part 2

    val gridInitPart2: Map[Loc, Position] = gridInit + (startLoc -> Pos(startLoc, 'a', false))
    val startingLocations: List[Loc] = gridInitPart2.values.filter(_.z == 'a').map(_.loc).toList
    val runs: List[Path] =
        startingLocations.map(start => findPath(gridInitPart2 + (start -> Start(start)), List(Path(List(Start(start)))))).filter(_.isDefined).map(_.get)

    val answer2: Int = runs.map(_.length - 1).min
    println(s"Result part 2: ${answer2}")
