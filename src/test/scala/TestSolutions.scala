import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:
  test("Day01") {
    assertResult(74394)(actual = Day01.answer1)
    assertResult(212836)(actual = Day01.answer2)
  }
  
  test("Day02") {
    assertResult(13682)(actual = Day02.answer1)
    assertResult(12881)(actual = Day02.answer2)
  }

  test("Day03") {
    assertResult(7763)(actual = Day03.answer1)
    assertResult(2569)(actual = Day03.answer2)
  }

  test("Day04") {
    assertResult(536)(actual = Day04.answer1)
    assertResult(845)(actual = Day04.answer2)
  }

  test("Day05") {
    assertResult("WSFTMRHPP")(actual = Day05.answer1)
    assertResult("GSLCMFBRP")(actual = Day05.answer2)
  }

  test("Day06") {
    assertResult(1198)(actual = Day06.answer1)
    assertResult(3120)(actual = Day06.answer2)
  }

  test("Day07") {
    assertResult(1428881)(actual = Day07.answer1)
    assertResult(10475598)(actual = Day07.answer2)
  }
  
  test("Day08") {
    assertResult(1763)(actual = Day08.answer1)
    assertResult(671160)(actual = Day08.answer2)
  }

  test("Day09") {
    assertResult(5878)(actual = Day09.answer1)
    assertResult(2405)(actual = Day09.answer2)
  }

  test("Day10") {
    assertResult(14920)(actual = Day10.answer1)
  }

  test("Day11") {
    assertResult(101436)(actual = Day11.answer1)
    //assertResult(19754471646)(actual = Day11.answer1)
  }
  
  test("Day12") {
    assertResult(380)(actual = Day12.answer1)
    assertResult(375)(actual = Day12.answer2)
  }

  test("Day13") {
    assertResult(6623)(actual = Day13.answer1)
    assertResult(23049)(actual = Day13.answer2)
  }
  
  test("Day14") {
    assertResult(755)(actual = Day14.answer1)
    assertResult(29805)(actual = Day14.answer2)
  }

  test("Day15") {
    assertResult(5832528)(actual = Day15.answer1)
    //assertResult(13360899249595)(actual = Day15.answer2)
  }

  test("Day18") {
    assertResult(3374)(actual = Day18.answer1)
    assertResult(2010)(actual = Day18.answer2)
  }
