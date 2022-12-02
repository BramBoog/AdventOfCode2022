object Day01 extends App:
    import scala.io.Source

    val caloriesPerElf: List[Int] =
        Source.fromResource("InputDay01.txt").mkString
        .split("\r\n\r\n").toList.map(_.split("\r\n").toList.map(_.toInt)).map(_.sum)
    
    // Part 1  
    val answer1: Int = caloriesPerElf.max
    println(s"Result part 1: ${answer1}")

    // Part 2
    val answer2: Int = caloriesPerElf.sorted.takeRight(3).sum
    println(s"Resutl part 2: ${answer2}")
