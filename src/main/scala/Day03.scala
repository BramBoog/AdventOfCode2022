object Day03 extends App:
    import scala.io.Source

    val input: List[String] =
        Source.fromResource("InputDay03.txt").getLines.toList

    def itemPriority(item: Char): Int =
        val num = item.asDigit - 9
        if (item.isLower) num
        else num + 26

    // Part 1

    val sharedItems: List[Char] =
        input
        .map(s => s.splitAt(s.length/2))
        .map((x,y) => x.toSet & y.toSet)
        .map(_.head)
        .toList

    val answer1: Int = sharedItems.map(itemPriority).sum
    println(s"Result part 1: ${answer1}")

    // Part 2

    val badges: List[Char] =
        input.grouped(3)
        .map(l => l.map(_.toSet))
        .map(l => l.tail.fold(l.head)(_&_))
        .map(_.head)
        .toList

    val answer2: Int = badges.map(itemPriority).sum
    println(s"Result part 2: ${answer2}")
