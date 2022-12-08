object Day07 extends App:
    import scala.io.Source

    val input: List[String] =
        Source.fromResource("InputDay07.txt").getLines.toList

    case class File(name: String, size: Int)

    case class Directory(name: String, files: List[File], directories: List[Directory]):
        val size: Int =
            files.map(_.size).sum + directories.map(_.size).sum
    
    def parseFileSystem(curDir: String, files: List[File], dirsMap: Map[String, Directory], commands: List[String]): (List[String], Directory) =
        if (!commands.headOption.isDefined) (List(), Directory(curDir, files, dirsMap.toList.map((_, d) => d)))
        else commands.head match
            case "$ cd .." => (commands.tail, Directory(curDir, files, dirsMap.toList.map((_, d) => d)))
            case s"${size} ${name}" if size.toIntOption.isDefined => parseFileSystem(curDir, files :+ File(name, size.toInt), dirsMap, commands.tail)
            case s"dir ${name}" => parseFileSystem(curDir, files, dirsMap + (name -> Directory(name, List(), List())), commands.tail)
            case s"$$ cd ${name}" if dirsMap.get(name).isDefined =>
                val (remCommands, dir) = parseFileSystem(name, List(), Map(), commands.tail)
                parseFileSystem(curDir, files, dirsMap + (name -> dir), remCommands)
            case s"$$ cd ${name}" => sys.error(s"cd into unknown directory: ${name}.")
            case "$ ls" => parseFileSystem(curDir, files, dirsMap, commands.tail)
            case _ => sys.error(s"Undefined file string: ${commands.head}.")

    val rootDir: Directory = parseFileSystem("/", List(), Map(), input.tail)._2

    // Part 1

    def sumOfSizesUnder10E5(dir: Directory): Int =
        if (dir.size <= 100000) dir.size + dir.directories.map(sumOfSizesUnder10E5).sum
        else dir.directories.map(sumOfSizesUnder10E5).sum
    
    val answer1: Int = sumOfSizesUnder10E5(rootDir)
    println(s"Result part 1: ${answer1}")

    // Part 2

    def getAllDirSizes(dir: Directory): List[Int] =        
        dir.directories.flatMap(getAllDirSizes) :+ dir.size

    val answer2: Int = getAllDirSizes(rootDir).filter(70000000 - rootDir.size + _ >= 30000000).min
    println(s"Result part 2: ${answer2}")
