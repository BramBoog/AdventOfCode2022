object Day13 extends App:
    import scala.io.Source

    sealed trait Element

    case class IntElement(v: Int) extends Element

    case class ListElement(contents: List[Element]) extends Element, Ordered[ListElement]:
        def tail: ListElement =
            ListElement(contents.tail)

        def compare(that: ListElement): Int =
            def compareOrder(leftL: ListElement, rightL: ListElement): Int =
                (leftL.contents.headOption, rightL.contents.headOption) match
                    case (None, None) => 0
                    case (None, Some(el)) => -1
                    case (Some(el), None) => 1
                    case (Some(leftEl), Some(rightEl)) => (leftEl, rightEl) match
                        case (IntElement(leftV), IntElement(rightV)) =>
                            if (leftV < rightV) -1
                            else if (leftV > rightV) 1
                            else compareOrder(leftL.tail, rightL.tail)
                        case (ListElement(leftList), ListElement(rightList)) =>
                            val result = compareOrder(ListElement(leftList), ListElement(rightList))
                            result match
                                case 0 => compareOrder(leftL.tail, rightL.tail)
                                case _ => result
                        case (IntElement(leftV), ListElement(_)) =>
                            compareOrder(ListElement(List(ListElement(List(IntElement(leftV)))) ::: leftL.contents.tail), rightL)
                        case (ListElement(_), IntElement(rightV)) =>
                            compareOrder(leftL, ListElement(List(ListElement(List(IntElement(rightV)))) ::: rightL.contents.tail))

            val result = compareOrder(this, that)
            if (result == 0) sys.error(s"Could not determine ordering of elements: ${this} and ${that}.")
            else result

    def parseElement(s: String, curListEl: ListElement): (String, ListElement) =
        if (s.isEmpty) (s, curListEl)
        else
            s.head match
                case '[' =>
                    val (remString, subListEl) = parseElement(s.tail, ListElement(List()))
                    parseElement(remString, ListElement(curListEl.contents :+ subListEl))
                case ']' => (s.tail, curListEl)
                case ',' => parseElement(s.tail, curListEl)
                case _ =>
                    val end =
                        if (s.indexOf(',') == -1) s.indexOf(']')
                        else List(s.indexOf(','), s.indexOf(']')).min
                    parseElement(s.drop(end), ListElement(curListEl.contents :+ IntElement(s.take(end).toInt)))

    val packets: List[ListElement] =
        Source.fromResource("InputDay13.txt")
        .getLines()
        .filter(!_.isEmpty)
        .toList
        .map(s => parseElement(s, ListElement(List()))._2)

    // Part 1

    val answer1 = packets.grouped(2).zipWithIndex.map((p, i) => (p == p.sorted, i+1)).filter(_._1).map(_._2).sum
    println(s"Result part 1: ${answer1}")

    // Part 2

    val dividerPackets: List[ListElement] = List("[[2]]", "[[6]]").map(s => parseElement(s, ListElement(List()))._2)

    val sortedPackets: List[ListElement] = packets.concat(dividerPackets).sorted

    val answer2 = (sortedPackets.indexOf(dividerPackets(0)) + 1) * (sortedPackets.indexOf(dividerPackets(1)) + 1)
    println(s"Result part 2: ${answer2}")
