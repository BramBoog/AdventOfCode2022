object Day20 extends App:
    import scala.io.Source

    case class Num(value: Int, mixed: Boolean)

    val input: List[Num] =
        Source.fromResource("InputDay20.txt").getLines.toList.map(n => Num(n.toInt, false))

    def mix(nums: List[Num]): List[Num] =
        nums.find(!_.mixed) match
            case None => nums
            case Some(num) =>
                val (numValTemp, numsTemp, oldIdx) =
                    if (num.value < 0) (-num.value, nums.reverse, nums.length - nums.indexOf(num) - 1)
                    else (num.value, nums, nums.indexOf(num))
                val newIdx = (oldIdx + numValTemp) % (nums.length - 1)
                val newList =
                    if (newIdx < oldIdx)
                        numsTemp.take(newIdx) ::: List(num.copy(mixed = true)) ::: numsTemp.slice(newIdx, oldIdx) ::: numsTemp.drop(oldIdx + 1)
                    else
                        numsTemp.take(oldIdx) ::: numsTemp.slice(oldIdx + 1, newIdx + 1) ::: List(num.copy(mixed = true)) ::: numsTemp.drop(newIdx + 1)
                
                if (num.value < 0) mix(newList.reverse)
                else mix(newList)

    val mixed: List[Num] = mix(input)

    val answer1: Int =
        def nthNumberAfter0(n: Int): Int =
            mixed((n + mixed.indexOf(Num(0, true))) % mixed.length).value
        nthNumberAfter0(1000) + nthNumberAfter0(2000) + nthNumberAfter0(3000)
    println(s"Result part 1: ${answer1}")

    println(input.groupBy(_.value).filter(_._2.length > 1))
