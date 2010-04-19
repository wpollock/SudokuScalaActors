import org.scalatest.FunSuite

class PuzzleTest extends FunSuite {

  test("finish a box") {
    val p = new Puzzle()
    p ! Put(1, 1, 1)
    p ! Put(1, 2, 2)
    p ! Put(1, 3, 3)
    p ! Put(2, 1, 4)
    p ! Put(2, 2, 5)
    p ! Put(2, 3, 6)
    p ! Put(3, 1, 7)
    p ! Put(3, 2, 8)
    Thread.sleep(1000)
    assert(p.getSquare(3, 3).value === 9)
  }

  test("finish a column") {
    val p = new Puzzle()
    p ! Put(1, 1, 1)
    p ! Put(2, 1, 2)
    p ! Put(3, 1, 3)
    p ! Put(4, 1, 4)
    p ! Put(5, 1, 5)
    p ! Put(6, 1, 6)
    p ! Put(7, 1, 7)
    p ! Put(8, 1, 8)
    Thread.sleep(1000)
    assert(p.getSquare(9, 1).value === 9)
  }

  test ("finish a row") {
    val p = new Puzzle()
    p ! Put(1, 1, 1)
    p ! Put(1, 2, 2)
    p ! Put(1, 3, 3)
    p ! Put(1, 4, 4)
    p ! Put(1, 5, 5)
    p ! Put(1, 6, 6)
    p ! Put(1, 7, 7)
    p ! Put(1, 8, 8)
    Thread.sleep(1000)
    assert(p.getSquare(1, 9).value === 9)
  }

  test("find an Only") {
    val p = new Puzzle()
    p ! Put(1, 1, 1)
    p ! Put(2, 4, 1)
    p ! Put(4, 7, 1)
    p ! Put(7, 8, 1)
    Thread.sleep(1000)
    assert(p.getSquare(3, 9).value === 1)
  }

  def noWhitespace(s: String) = s.replaceAll("\\s", "")
 
  def doAPuzzle(in: String, solution: String) = {
    val p = new Puzzle()
    p ! Load(in)
    Thread.sleep(2000)
    println(p.printString)
    val pSolved = noWhitespace(p.printString)
    assert(pSolved.length === 81)
    assert(pSolved === noWhitespace(solution))
  }

  test("Solve puzzle #1") {
    val puzzle = """
      530070000
      600195000
      098000060
      800060003
      400803001
      700020006
      060000280
      000419005
      000080079"""
    val solution = """
      534678912
      672195348
      198342567
      859761423
      426853791
      713924856
      961537284
      287419635
      345286179"""
    doAPuzzle(puzzle, solution)
  }

  test("Solve puzzle #2") {
    val puzzle = """
      000094100
      009008000
      020000807
      072005080
      000000000
      030700560
      806000020
      000400700
      007510000"""
    val solution = """
      783694152
      159278643
      624351897
      972165384
      561843279
      438729561
      816937425
      395482716
      247516938"""
    doAPuzzle(puzzle, solution)
  }

}


/*
 
  val input3 = """
    090002050
    000075300
    004300000
    180030040
    709000503
    040010067
    000007100
    006190000 begin_of_the_skype_highlighting              006190000      end_of_the_skype_highlighting
    030600070"""

*/
