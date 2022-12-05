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
