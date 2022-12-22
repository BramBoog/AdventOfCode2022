object Day17 extends App:
    import scala.io.Source

    val jetPattern: List[Char] =
        Source.fromResource("ExampleDay17.txt").getLines.toList.head.toList

    case class Pos(x: Int, y: Long)

    case class Rock(positions: Set[Pos], isMoving: Boolean = true):
        def moveOnTick(tick: Int, chamber: Set[Pos]): Rock =
            val positionsAfterJet =
                val attemptedMove =
                    jetPattern(tick) match
                        case '<' if !positions.exists(_.x == 1) => positions.map(p => Pos(p.x - 1, p.y))
                        case '>' if !positions.exists(_.x == 7) => positions.map(p => Pos(p.x + 1, p.y))
                        case _ => positions

                if (chamber.exists(attemptedMove.contains)) positions
                else attemptedMove

            val attemptedFall = positionsAfterJet.map(p => Pos(p.x, p.y - 1))
            if (chamber.exists(attemptedFall.contains)) Rock(positionsAfterJet, false)
            else this.copy(positions = attemptedFall)

    object Rock:
        def spawn(shape: String, maxYInChamber: Long): Rock =
            shape match
                case "horizontal" => Rock((3 to 6).toSet.map(x => Pos(x, maxYInChamber + 4)))
                case "plus" => Rock(Set(Pos(3, maxYInChamber + 5), Pos(4, maxYInChamber + 6), Pos(4, maxYInChamber + 5), Pos(4, maxYInChamber + 4), Pos(5, maxYInChamber + 5)))
                case "L" => Rock((3 to 5).toSet.map(x => Pos(x, maxYInChamber + 4)) | Set(Pos(5, maxYInChamber + 5), Pos(5, maxYInChamber + 6)))
                case "vertical" => Rock((maxYInChamber + 4 to maxYInChamber + 7).toSet.map(y => Pos(3, y)))
                case "square" => Rock(Set(Pos(3, maxYInChamber + 4), Pos(3, maxYInChamber + 5), Pos(4, maxYInChamber + 5), Pos(4, maxYInChamber + 4)))

    val rocksInOrder: List[String] = List("horizontal", "plus", "L", "vertical", "square")

    def dropRocksUntil(cutoff: Long, chamber: Set[Pos], curRock: Rock, tick: Int = 0, rockNumber: Long = 0): Set[Pos] =
        if (rockNumber == cutoff) chamber 
        else if (!curRock.isMoving)
            val newChamber =
                val added = chamber | curRock.positions
                val fullLines =
                    for y <- curRock.positions.map(_.y) if added.filter(_.y == y).size == 7
                    yield y
                if (!fullLines.isEmpty) added.filter(_.y >= fullLines.max)
                else added
            dropRocksUntil(cutoff, newChamber, Rock.spawn(rocksInOrder(((rockNumber + 1) % rocksInOrder.length).toInt), newChamber.map(_.y).max), tick % jetPattern.length, rockNumber + 1)
        else dropRocksUntil(cutoff, chamber, curRock.moveOnTick(tick, chamber), (tick + 1) % jetPattern.length, rockNumber)
                
    val chamberInit: Set[Pos] = (1 to 7).toSet.map(x => Pos(x, 0))

    val start1: Long = System.currentTimeMillis

    val answer1: Long = dropRocksUntil(2022, chamberInit, Rock.spawn(rocksInOrder(0), 0)).map(_.y).max
    println(s"Result part 1: ${answer1} in ${System.currentTimeMillis - start1}ms")

    // val start2: Long = System.currentTimeMillis

    // val answer2: Long = dropRocksUntil(1000000000000L, chamberInit, Rock.spawn(rocksInOrder(0), 0)).map(_.y).max
    // println(s"Result part 2: ${answer2} in ${System.currentTimeMillis - start2}ms")
    
    // Naive: 5510ms
    // With max of rock filled: 5619ms
