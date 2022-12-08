object Day08 extends App:
    import scala.io.Source

    val input: List[List[Int]] =
        Source.fromResource("InputDay08.txt").getLines.toList.map(_.toList.map(_.toInt))

    def findVisibleInGrid(treeGrid: List[List[Int]]): Set[(Int, Int)] =
        def visibleInLine(visible: Set[Int], trees: List[Int], i: Int, incr: Int => Int, max: Int = -1): Set[Int] =
            if (trees.isEmpty) visible
            else if (trees.head > max) visibleInLine(visible.incl(i), trees.tail, incr(i), incr, trees.head)
            else visibleInLine(visible, trees.tail, incr(i), incr, max)

        val sets = 
            for (line, i) <- treeGrid.zipWithIndex if i != 0 & i != treeGrid.length - 1
            yield (visibleInLine(Set(), line, 0, _+1) ++ visibleInLine(Set(), line.reverse, line.length - 1, _-1)).map(j => (i, j))

        sets.tail.fold(sets.head)(_++_)

    val answer1: Int = (findVisibleInGrid(input) ++ findVisibleInGrid(input.transpose).map((j,i) => (i,j))).size + 4 // + 4 corners
    println(s"Result part 1: ${answer1}")

    def treeScoresHorizontal(treeGrid: List[List[Int]]): List[((Int, Int), Int)] =
        def countVisibleInLineFromTree(height: Int, trees: List[Int], count: Int = 0): Int =
            if (trees.isEmpty) count
            else if (trees.head >= height) count + 1
            else countVisibleInLineFromTree(height, trees.tail, count+1)

        for
            (line, i) <- treeGrid.zipWithIndex
            (tree, j) <- line.zipWithIndex
        yield ((i,j), countVisibleInLineFromTree(tree, line.drop(j + 1)) * countVisibleInLineFromTree(tree, line.reverse.drop(line.length - j)))

    val answer2: Int = (treeScoresHorizontal(input) ::: treeScoresHorizontal(input.transpose).map((ij, s) => ((ij._2, ij._1), s))).groupBy(_._1).values.map(_.map(_._2).product).max
    println(s"Result part 2: ${answer2}")
