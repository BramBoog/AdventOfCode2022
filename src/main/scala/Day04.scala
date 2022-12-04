object Day04 extends App:
    import scala.io.Source

    case class Range(start: Int, end: Int)

    def parse(s: String): (Range, Range) =
        s match
            case s"${a}-${b},${c}-${d}" => (Range(a.toInt,b.toInt), Range(c.toInt,d.toInt))
            case _ => sys.error(s"Undefined input: ${s}") 

    val pairs: List[(Range, Range)] =
        Source.fromResource("InputDay04.txt").getLines.toList
        .map(parse)

    // Part 1

    val answer1: Int =
        pairs
        .filter((r1, r2) => (r1.start >= r2.start & r1.end <= r2.end) | (r1.start <= r2.start & r1.end >= r2.end))
        .length
    println(s"Result part 1: ${answer1}")

    // Part 2

    val answer2: Int =
        pairs
        .filter((r1, r2) =>
            (r2.start <= r1.start & r1.start <= r2.end) |
            (r2.start <= r1.end & r1.end <= r2.end) |
            (r1.start <= r2.start & r2.start <= r1.end) |
            (r1.start <= r2.end & r2.end <= r1.end)
        ).length
    println(s"Result part 2: ${answer2}")
