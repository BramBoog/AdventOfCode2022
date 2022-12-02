object Day02 extends App:
    import scala.io.Source
    
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

    val instructions: List[(String, String)] =
        Source.fromResource("InputDay02.txt").getLines.toList.map(_.split(" ")).map({case Array(x,y) => (x,y)})
    
    // Part 1

    val moveCodes: Map[String, String] =
        Map(
            ("A" -> "rock"),
            ("B" -> "paper"),
            ("C" -> "scissors"),
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

    def computeRoundScore(opponent: String, you: String): Int =
        val opDec = moveCodes.get(opponent).get
        val youDec = moveCodes.get(you).get
        roundScores.get(findResult(opDec, youDec)).get
        +
        moveScores.get(youDec).get

    val answer1: Int = instructions.map((x,y) => computeRoundScore(x,y)).sum
    println(s"Result part 1: ${answer1}")

    // Part 2

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

    def computeRoundScore2(opponent: String, result: String): Int =
        val opDec = moveCodes.get(opponent).get
        val resultDec = resultCodes.get(result).get
        roundScores.get(resultDec).get
        +
        moveScores.get(findMove(opDec, resultDec)).get

    val answer2: Int = instructions.map((x,y) => computeRoundScore2(x,y)).sum
    println(s"Result part 2: ${answer2}")
