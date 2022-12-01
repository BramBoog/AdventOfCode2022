object Day01 extends App:
    import scala.io.Source

    val calories: List[String] = Source.fromResource("InputDay01.txt").getLines.toList

    def countUpCalories(inputList: List[String], groupedCals: List[Int], curGroup: Int = 0): List[Int] =
        inputList.headOption match
            case None => groupedCals
            case Some("") => countUpCalories(inputList.tail, groupedCals.appended(curGroup))
            case Some(x) if x.toIntOption.isDefined => countUpCalories(inputList.tail, groupedCals, curGroup + x.toInt)
            case _ => sys.error(s"Cannot convert input: ${inputList.head} to Int.")

    val groupedCalories: List[Int] = countUpCalories(calories, List())

    // Part 1  
    val answer1: Int = groupedCalories.max
    println(s"Result part 1: ${answer1}")

    // Part 2
    def addMaxThree(l: List[Int], n: Int = 0, total: Int = 0): Int =
        if (n < 3) addMaxThree(l.diff(List(l.max)), n + 1, total + l.max)
        else total

    val answer2: Int = addMaxThree(groupedCalories)
    println(s"Resutl part 2: ${answer2}")
