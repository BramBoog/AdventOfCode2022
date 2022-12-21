object Day17 extends App:
    import scala.io.Source

    val jetPattern: List[Char] =
        Source.fromResource("InputDay17.txt").getLines.toList.head.toList

    case class Pos(x: Int, y: Int)

    sealed trait Rock:
        def positions: Set[Pos]
        def isMoving: Boolean

        def moveOnTick(tick: Int, chamber: Set[Pos]): (Rock, Set[Pos]) =
            ???
            // val positionsAfterJet =
            //     val attemptedMove =
            //         jetPattern(tick % jetPattern.length) match
            //             case '<' if !positions.exists(_.x == 0) => positions.map(p => Pos(p.x - 1, p.y))
            //             case '>' if !positions.exists(_.x == 6) => positions.map(p => Pos(p.x + 1, p.y))

            //     if (chamber.exists(attemptedMove.contains)) positions
            //     else attemptedMove

            // val attemptedFall = positionsAfterJet.map(p => Pos(p.x, p.y - 1))
            // if (chamber.exists(attemptedFall.contains)) (copy(this, positionsAfterJet, false), chamber | positionsAfterJet)

    case class HorizontalLine(positions: Set[Pos], isMoving: Boolean = false) extends Rock
