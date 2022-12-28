object Day18 extends App:
    import scala.io.Source
    import scala.math

    sealed trait Loc:
        def x: Int
        def y: Int
        def z: Int

        def distanceTo(p: Pos): Int =
            math.sqrt(math.pow(x - p.x, 2) + math.pow(y - p.y, 2) + math.pow(z - p.z, 2)).toInt

        def possibleMoves(positionsOffLimits: Set[Pos]): List[Pos] =
            val deltas = List(-1, 0, 1)
            for
                dx <- deltas
                dy <- deltas
                dz <- deltas
                if math.abs(dx) + math.abs(dy) + math.abs(dz) == 1
                    && !positionsOffLimits.contains(Pos(x + dx, y + dy, z + dz))
            yield
                Pos(x + dx, y + dy, z + dz)

    case class Pos(x: Int, y: Int, z: Int) extends Loc

    case class Cube(x: Int, y: Int, z: Int) extends Loc:
        def sidesNotCovered(cubes: Set[Cube]): Int =
            6 - cubes.filter(c => math.abs(c.x - x) + math.abs(c.y - y) + math.abs(c.z - z) == 1).size

        def sidesOnOutside(cubes: Set[Cube]): Int =
            val minX = cubes.map(_.x).min
            val maxX = cubes.map(_.x).max
            val minY = cubes.map(_.y).min
            val maxY = cubes.map(_.y).max
            val minZ = cubes.map(_.z).min
            val maxZ = cubes.map(_.z).max

            def existsPathToOutside(curPath: List[Pos], exploredPos: Set[Pos]): (Boolean, Set[Pos]) =
                // Using depth-first algorithm, which is in this case much more efficient than breadth-first
                val newPaths =
                    for next <- curPath.head.possibleMoves(exploredPos)
                    yield List(next) ::: curPath
                val newExplored = exploredPos | newPaths.map(_.head).toSet

                if (newPaths.map(_.head).exists(p => p.x < minX || p.x > maxX || p.y < minY || p.y > maxY || p.z < minZ || p.z > maxZ)) (true, newExplored)
                else
                    def checkPaths(paths: List[List[Pos]], exploredPos1: Set[Pos]): (Boolean, Set[Pos]) =
                        if (paths.isEmpty) (false, exploredPos1)
                        else 
                            val (success, explored) = existsPathToOutside(paths.head, exploredPos1)
                            if (success) (true, explored)
                            else checkPaths(paths.tail, explored)

                    /* Considered sorting newPaths by how close the head Pos is to any of the edges (the goal), which could potentially help to
                    reach the exit condition of checkPaths faster, but it ended up only taking longer in total. */
                    checkPaths(newPaths, newExplored)

            val cubesPos = cubes.map(c => Pos(c.x, c.y, c.z))
            possibleMoves(cubesPos).map(p => existsPathToOutside(List(p), cubesPos.incl(p))._1).count(_ == true)

    val allCubes: Set[Cube] =
        Source.fromResource("InputDay18.txt").getLines.toSet
        .map({
            case s"$x,$y,$z" => Cube(x.toInt, y.toInt, z.toInt)
        })

    val answer1: Int = allCubes.toList.map(_.sidesNotCovered(allCubes)).sum
    println(s"Result part 1: ${answer1}")


    val answer2: Int = allCubes.toList.map(_.sidesOnOutside(allCubes)).sum
    println(s"Result part 2: ${answer2}")
