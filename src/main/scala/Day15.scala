object Day15 extends App:
    import scala.io.Source
    import scala.math

    case class Pos(x: Int, y: Int):
        def distance(that: Pos): Int =
            math.abs(that.x - x) + math.abs(that.y - y)
    
    case class Line(min: Int, max: Int)

    case class Sensor(pos: Pos, closestBeacon: Pos):
        val distanceToBeacon: Int = pos.distance(closestBeacon)

        def exclusionZoneOnY(y: Int): Option[Line] =
            val lineDistance = pos.distance(Pos(pos.x, y))
            if (distanceToBeacon < lineDistance) None
            else
                val diff = distanceToBeacon - lineDistance
                Some(Line(pos.x - diff, pos.x + diff))

    def parseSensor(s: String): Sensor =
        s match
            case s"Sensor at x=${sx}, y=${sy}: closest beacon is at x=${bx}, y=${by}" => Sensor(Pos(sx.toInt, sy.toInt), Pos(bx.toInt, by.toInt))
            case _ => sys.error(s"Unable to parse sensor from input ${s}.")
        
    val sensors: List[Sensor] =
        Source.fromResource("InputDay15.txt").getLines.toList.map(parseSensor)

    val sensorAndBeaconPositions: Set[Pos] =
        sensors.foldLeft(Set())((set, sensor) => set | Set(sensor.pos, sensor.closestBeacon))

    val size: Int = 4000000

    // Part 1

    val answer1: Int =
        val sensorAndBeaconPositions: Set[Int] = sensors.foldLeft(Set())((set, sensor) => set | Set(sensor.pos, sensor.closestBeacon).filter(_.y == size / 2).map(_.x))
        sensors.flatMap(_.exclusionZoneOnY(size / 2))
        .map(l => Range(l.min, l.max + 1).toSet)
        .fold(Set())((acc, set) => acc | set)
        .removedAll(sensorAndBeaconPositions)
        .size
    println(s"Result part 1: ${answer1}")

    // Part 2

    def checkLine(y: Int, excludedRangesSorted: List[Line], x: Int = 0): Option[Pos] =
        if (x >= size) None
        else if (x < excludedRangesSorted.head.min)
            Some(Pos(x, y))
        else checkLine(y, excludedRangesSorted.tail.dropWhile(_.max <= excludedRangesSorted.head.max), excludedRangesSorted.head.max + 1)
    
    val answer2: Long =
        val positionNotExcluded =
            (
            for y <- (1 to size)
                if (checkLine(y, sensors.flatMap(_.exclusionZoneOnY(y)).sortBy(_.min))).isDefined
            yield (checkLine(y, sensors.flatMap(_.exclusionZoneOnY(y)).sortBy(_.min))).get
            ).head
        positionNotExcluded.x.toLong * 4000000 + positionNotExcluded.y
    println(s"Result part 2: ${answer2}")
