object Day11 extends App:
    import scala.io.Source
    
    case class Monkey(number: Int, items: List[Long], op: Long => Long, divisibleBy: Int, monkeyIfTrue: Int, monkeyIfFalse: Int, amountInspected: Long = 0):
        def toWhichMonkey(wl: Long): Int =
            if (wl % divisibleBy == 0) monkeyIfTrue
            else monkeyIfFalse

        def inspectAndThrowItem(monkeyMap: Map[Int, Monkey], afterInspection: Long => Long): Map[Int, Monkey] =
            val newItem = afterInspection(op(items.head)) % divisibleByProduct
            val toMonkey = toWhichMonkey(newItem)
            monkeyMap.get(toMonkey) match
                case None => sys.error(s"Couldn't find Monkey #${toMonkey} in the map.")
                case Some(m) => monkeyMap + (toMonkey -> m.copy(items = m.items ::: List(newItem))) + (number -> this.copy(items = items.tail, amountInspected = amountInspected + 1)) 

    def parseMonkey(strings: List[String]): Monkey =
        if (strings.length != 6) sys.error(s"Input Array: ${strings} did not contain the right amount of elements to parse into a Monkey.")
        else
            val number: Int =
                strings(0) match
                    case s"Monkey ${n}:" => n.toInt
            val items: List[Long] =
                strings(1) match
                    case s"  Starting items: ${items}" => items.split(", ").map(_.toLong).toList
            val op: Long => Long =
                strings(2) match
                    case s"  Operation: new = old * old" => n => n * n
                    case s"  Operation: new = old * ${x}" => _ * x.toLong
                    case s"  Operation: new = old + ${x}" => _ + x.toLong
            val divisibleBy: Int =
                strings(3) match
                    case s"  Test: divisible by ${x}" => x.toInt
            val monkeyIfTrue: Int =
                strings(4) match
                    case s"    If true: throw to monkey ${x}" => x.toInt
            val monkeyIfFalse: Int =
                strings(5) match
                    case s"    If false: throw to monkey ${x}" => x.toInt

            Monkey(number, items, op, divisibleBy, monkeyIfTrue, monkeyIfFalse)          
        
    val monkeys: List[Monkey] =
        Source.fromResource("InputDay11.txt")
        .mkString.split("\r\n\r\n").toList
        .map(_.split("\r\n").toList)
        .map(parseMonkey)

    val divisibleByProduct: Int = monkeys.map(_.divisibleBy).product
    val lastMonkey: Int = monkeys.map(_.number).max

    def advance(monkeyMap: Map[Int, Monkey], afterInspection: Long => Long, cutoff: Int, round: Int = 0, curMonkey: Int = 0): List[Monkey] =
        if (round == cutoff) monkeyMap.values.toList
        else
            val monkeyOnTurn = monkeyMap.get(curMonkey).get
            monkeyOnTurn.items.headOption match
                case None =>
                    if (curMonkey == lastMonkey) advance(monkeyMap, afterInspection, cutoff, round + 1, 0)
                    else advance(monkeyMap, afterInspection, cutoff, round, curMonkey + 1)
                case Some(item) => advance(monkeyOnTurn.inspectAndThrowItem(monkeyMap, afterInspection), afterInspection, cutoff, round, curMonkey)
    
    // Part 1
    val answer1: Long = advance(monkeys.map(m => (m.number, m)).toMap, _ / 3, 20).map(_.amountInspected).sorted.reverse.take(2).product
    println(s"Result part 1: ${answer1}")

    // Part 2
    val answer2: Long = advance(monkeys.map(m => (m.number, m)).toMap, identity, 10000).map(_.amountInspected).sorted.reverse.take(2).product
    println(s"Result part 2: ${answer2}")
