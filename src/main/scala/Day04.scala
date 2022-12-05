object Day04 extends App:
    import scala.io.Source

    def parse(s: String): (Range, Range) =
        s match
            case s"${a}-${b},${c}-${d}" => (Range(a.toInt, b.toInt+1), Range(c.toInt, d.toInt+1))
            case _ => sys.error(s"Undefined input: ${s}") 

    val pairs: List[(Range, Range)] =
        Source.fromResource("InputDay04.txt").getLines.toList
        .map(parse)

    // Part 1

    val answer1: Int = pairs.count((r1, r2) => (r1.contains(r2.start) & r1.contains(r2.end-1)) | (r2.contains(r1.start) & r2.contains(r1.end-1)))
    println(s"Result part 1: ${answer1}")

    // Part 2

    val answer2: Int = pairs.count((r1, r2) => r1.exists(i => r2.contains(i)))
    println(s"Result part 2: ${answer2}")
