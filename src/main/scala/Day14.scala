object Day14 extends App:
    import scala.io.Source

    def createRockCoordinates(start: Array[Int], end: Array[Int]): List[(Int, Int)] =
        if (start(0) == end(0))
            val line =
                if (start(1) < end(1)) Range(start(1), end(1) + 1).toList
                else Range(start(1), end(1) - 1, -1).toList
            line.zip(List.fill(line.length)(start(0))).map((y,x) => (x,y))
        else if (start(1) == end(1))
            val line = 
                if (start(0) < end(0)) Range(start(0), end(0) + 1).toList
                else Range(start(0), end(0) - 1, -1).toList
            line.zip(List.fill(line.length)(start(1)))
        else sys.error(s"Start: ${start} and end: ${end} have no coordinate in common")

    val rockCoordinates =
        Source.fromResource("ExampleDay14.txt").getLines.toList
        .map(_
            .split(" -> ").map(_.split(",").map(_.toInt))
            .sliding(2)
            .map(a => createRockCoordinates(a(0), a(1)))
        ).flatten.flatten.toSet

    println(rockCoordinates)
