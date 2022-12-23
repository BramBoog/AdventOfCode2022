object Day17 extends App:
    import scala.io.Source

    val jetPattern: List[Char] =
        Source.fromResource("InputDay17.txt").getLines.toList.head.toList

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

    def dropRocksUntil(cutoff: Long, chamber: Set[Pos], curRock: Rock, rocksInOrder: List[String], tick: Int = 0, rockNumber: Long = 0): Set[Pos] =
        if (rockNumber == 37)
            println(curRock.positions)
            Set()
        if (rockNumber == cutoff) chamber 
        else if (!curRock.isMoving)
            val newChamber =
                val added = chamber | curRock.positions
                val newMaxY = added.map(_.y).max
                val xsOnMaxY = added.filter(_.y == newMaxY).map(_.x)
                if (xsOnMaxY.size == 7) println(s"Full line at: $newMaxY")
                val newRock = Rock.spawn(rocksInOrder.head, newMaxY)
                val sizeAtBottom = newRock.positions.filter(_.y == newRock.positions.map(_.y).min).size
                if ((1 to 8 - sizeAtBottom).exists(x => ((x until x + sizeAtBottom).toSet & xsOnMaxY).size == 0))
                    added
                else
                    println(s"Line too full for next rock at: $newMaxY")
                    println(added)
                    println(sizeAtBottom)
                    println(added.filter(_.y >= newMaxY))
                    added.filter(_.y >= newMaxY)
            if (rockNumber == 36)
                println(newChamber)
            if (rockNumber % 1000000 == 0) println(rockNumber)
            dropRocksUntil(cutoff, newChamber, Rock.spawn(rocksInOrder.head, newChamber.map(_.y).max), rocksInOrder.tail :+ rocksInOrder.head, tick % jetPattern.length, rockNumber + 1)
        else dropRocksUntil(cutoff, chamber, curRock.moveOnTick(tick, chamber), rocksInOrder, (tick + 1) % jetPattern.length, rockNumber)
                
    val chamberInit: Set[Pos] = (1 to 7).toSet.map(x => Pos(x, 0))
    val rocksInOrderInit: List[String] = List("horizontal", "plus", "L", "vertical", "square")

    val start1: Long = System.currentTimeMillis

    val answer1: Long = dropRocksUntil(2022, chamberInit, Rock.spawn(rocksInOrderInit.head, 0), rocksInOrderInit.tail :+ rocksInOrderInit.head).map(_.y).max
    println(s"Result part 1: ${answer1} in ${System.currentTimeMillis - start1}ms")

    // val start2: Long = System.currentTimeMillis

    // val answer2: Long = dropRocksUntil(1000000000000L, chamberInit, Rock.spawn(rocksInOrderInit.head, 0), rocksInOrderInit.tail :+ rocksInOrderInit.head).map(_.y).max
    // println(s"Result part 2: ${answer2} in ${System.currentTimeMillis - start2}ms")
    
    // Part 1: 601ms
