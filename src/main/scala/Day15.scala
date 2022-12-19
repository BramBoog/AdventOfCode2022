object Day15 extends App:
    import scala.io.Source
    import scala.math

    case class Pos(x: Int, y: Int):
        def distance(that: Pos): Int =
            math.abs(that.x - x) + math.abs(that.y - y)

    case class Sensor(pos: Pos, closestBeacon: Pos):
        def exclusionZoneInLine(y: Int): Set[Pos] =
            val beaconDistance = pos.distance(closestBeacon)
            val lineDistance = pos.distance(Pos(pos.x, y))
            if (beaconDistance < lineDistance) Set()
            else
                val diff = beaconDistance - lineDistance
                val lineInX = Range(pos.x - diff, pos.x + diff + 1)
                lineInX.map(x => Pos(x, y)).toSet -- sensorAndBeaconPositions
        
        def exclusionZone: Set[Pos] =
            val maxDistance = pos.distance(closestBeacon)
            for x <- Range(pos.x - maxDistance, pos.x + maxDistance + 1)
            yield Range()

            val octant1 = Range(0, maxDistance + 1).combinations(2).map(l => Pos(l(0), l(1))).toSet
            val octant2 = octant1.map(p => Pos(p.y, p.x))
            val middleLine = (for i <- Range(1, maxDistance / 2 + 1) yield Pos(i, i)).toSet
            val positiveQuadrant = (octant1 | octant2).filter(_.distance(Pos(0,0)) <= maxDistance) | middleLine
            (
                positiveQuadrant
                | positiveQuadrant.map(p => Pos(-p.x, p.y))
                | positiveQuadrant.map(p => Pos(p.x, -p.y))
                | positiveQuadrant.map(p => Pos(-p.x, -p.y))
            )
            .map(delta => Pos(pos.x + delta.x, pos.y + delta.y))

    def parseSensor(s: String): Sensor =
        s match
            case s"Sensor at x=${sx}, y=${sy}: closest beacon is at x=${bx}, y=${by}" => Sensor(Pos(sx.toInt, sy.toInt), Pos(bx.toInt, by.toInt))
            case _ => sys.error(s"Unable to parse sensor from input ${s}.")
        
    val sensors: List[Sensor] =
        Source.fromResource("InputDay15.txt").getLines.toList.map(parseSensor)

    val sensorAndBeaconPositions: Set[Pos] =
        sensors.foldLeft(Set())((set, sensor) => set | Set(sensor.pos, sensor.closestBeacon))

    val answer1 = sensors.foldLeft(Set())((set: Set[Pos], sensor) => set | (sensor.exclusionZoneInLine(2000000))).size
    println(s"Result part 1: ${answer1}")
