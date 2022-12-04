object Day02 extends App:
    import scala.io.Source
 
    val instructions: List[(Char, Char)] =
        Source.fromResource("InputDay02.txt").getLines.toList
        .map(_.split(" ")).map({case Array(x,y) => (x.head,y.head)})
    
    // Part 1

    def findScore1(i1: Char, i2: Char): Int =
        (i1, i2) match
            case ('A', 'X') => 3 + 1
            case ('A', 'Y') => 6 + 2
            case ('A', 'Z') => 0 + 3
            case ('B', 'X') => 0 + 1
            case ('B', 'Y') => 3 + 2
            case ('B', 'Z') => 6 + 3
            case ('C', 'X') => 6 + 1
            case ('C', 'Y') => 0 + 2
            case ('C', 'Z') => 3 + 3

    val answer1: Int = instructions.map((x,y) => findScore1(x, y)).sum
    println(s"Result part 1: ${answer1}")

    // Part 2

    def findScore2(i1: Char, i2: Char): Int =
        (i1, i2) match
            case ('A', 'X') => 0 + 3
            case ('A', 'Y') => 3 + 1
            case ('A', 'Z') => 6 + 2
            case ('B', 'X') => 0 + 1
            case ('B', 'Y') => 3 + 2
            case ('B', 'Z') => 6 + 3
            case ('C', 'X') => 0 + 2
            case ('C', 'Y') => 3 + 3
            case ('C', 'Z') => 6 + 1

    val answer2: Int = instructions.map((x,y) => findScore2(x, y)).sum
    println(s"Result part 2: ${answer2}")
