object Day02 extends App:
    import scala.io.Source
<<<<<<< HEAD
    
    val moveScores: Map[String, Int] =
        Map(
            ("rock" -> 1),
            ("paper" -> 2),
            ("scissors" -> 3)
        )

    val roundScores: Map[String, Int] =
        Map(
            ("loss" -> 0),
            ("draw" -> 3),
            ("win" -> 6)
        )
    
    val moveCodesOpponent: Map[String, String] =
        Map(
            ("A" -> "rock"),
            ("B" -> "paper"),
            ("C" -> "scissors")
        )
    
    def computeRoundScore(opponent: String, input2: String, input2Type: String, input2Codex: Map[String, String]): Int =
        val opDec = moveCodesOpponent.get(opponent).get
        val input2Dec = input2Codex.get(input2).get
        input2Type match
            case "move" => roundScores.get(findResult(opDec, input2Dec)).get + moveScores.get(input2Dec).get
            case "result" => roundScores.get(input2Dec).get + moveScores.get(findMove(opDec, input2Dec)).get
 
    val instructions: List[(String, String)] =
        Source.fromResource("InputDay02.txt").getLines.toList.map(_.split(" ")).map({case Array(x,y) => (x,y)})
    
    // Part 1

    val moveCodesYou: Map[String, String] =
        Map(
            ("X" -> "rock"),
            ("Y" -> "paper"),
            ("Z" -> "scissors")
        )

    def findResult(opponent: String, you: String): String =
        (opponent, you) match
            case (x,y) if x==y => "draw"
            case ("rock", "paper") => "win"
            case ("paper", "scissors") => "win"
            case ("scissors", "rock") => "win"
            case ("paper", "rock") => "loss"
            case ("scissors", "paper") => "loss"
            case ("rock", "scissors") => "loss"
            case _ => sys.error(s"Undefined input: ${(opponent, you)}")

    val answer1: Int = instructions.map((x,y) => computeRoundScore(x, y, "move", moveCodesYou)).sum
=======
 
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
>>>>>>> main
    println(s"Result part 1: ${answer1}")

    // Part 2

<<<<<<< HEAD
    val resultCodes: Map[String, String] =
        Map(
            ("X" -> "loss"),
            ("Y" -> "draw"),
            ("Z" -> "win")
        )

    def findMove(opponent: String, result: String): String =
        (opponent, result) match
            case ("rock", "win") => "paper"
            case ("rock", "draw") => "rock"
            case ("rock", "loss") => "scissors"
            case ("paper", "win") => "scissors"
            case ("paper", "draw") => "paper"
            case ("paper", "loss") => "rock"
            case ("scissors", "win") => "rock"
            case ("scissors", "draw") => "scissors"
            case ("scissors", "loss") => "paper"
            case _ => sys.error(s"Undefined input: ${(opponent, result)}")

    val answer2: Int = instructions.map((x,y) => computeRoundScore(x, y, "result", resultCodes)).sum
=======
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
>>>>>>> main
    println(s"Result part 2: ${answer2}")
