object Day06 extends App:
    import scala.io.Source

    val input: String =
        Source.fromResource("InputDay06.txt").mkString

    def findMarkerIndex(n: Int): Int =
        input.sliding(n).map(_.toSet).indexWhere(_.size == n) + n

    val answer1: Int = findMarkerIndex(4)
    println(s"Result part 1: ${answer1}")

    val answer2: Int = findMarkerIndex(14)
    println(s"Result part 2: ${answer2}")
    