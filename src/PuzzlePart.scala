import scala.actors.Actor
import Actor._

// Messages

abstract case class SudokuMessage()
case class Set(sq: PuzzleSquare, n: Int, from: PuzzlePart) extends SudokuMessage
case class Not(sq: PuzzleSquare, n: Int, from: PuzzlePart) extends SudokuMessage
case class Finished(p: PuzzlePart) extends SudokuMessage
case class Put(r: Int, c: Int, n: Int) extends SudokuMessage
case class Load(s: String) extends SudokuMessage

// Immutable square state
case class SquareState(val value: Int, val flags: Int) {

  import SquareState._

  // Turn off one possible value for this square
  def not(n: Int) =
    if (finished)
      this
    else
      SquareState(0, flags & notFlags(n)).check

  // Check this square state to see if it's finished
  private def check =
    finishedStates.find(_.flags == flags) match {
      case Some(st) => st
      case _ => this
    }

  // Is flag 'n' still on? (Could value 'n' still belong in this square?)
  def isFlagOn(n: Int) = 0 != (flags & nFlag(n))

  // True if the value is set (not zero)
  def finished = 0 != value
}

object SquareState {
  // Flags value with all bits set
  val flags_all = 0x1ff

  // Flag mask with only bit 'n' set
  def nFlag(n: Int) = 1 << (n - 1)

  // Flag mask with all bits set except 'n' bit
  def notFlags(n: Int) = flags_all ^ nFlag(n)

  // Default starting state
  val default = SquareState(0, flags_all)

  // The nine possible finished states, one for each possible square value
  val finishedStates = (1 to 9).map(n => SquareState(n, nFlag(n))).toArray
  def getFinishedState(n: Int) = finishedStates(n - 1)
}

// Actors

// Any subset of a puzzle
abstract class PuzzlePart extends Actor {
  def finished: Boolean
  def handleMessage: PartialFunction[SudokuMessage, Unit]

  def act = loop {
    react {
      case m: SudokuMessage if (!finished) => {
        println(this + ": " + m)
        handleMessage(m)
      }
    }
  }
}

// An individual puzzle square
class PuzzleSquare(val row: PuzzleGroup, val column: PuzzleGroup, val box: PuzzleGroup) extends PuzzlePart {
  var state: SquareState = SquareState.default
  // Expose two state methods
  def finished = state.finished
  def value = state.value

  // List of all groups to which this square belongs
  val groups = List(row, column, box)

  override def toString = List("Square(", row.num, ",", column.num, ")").mkString

  def handleMessage = {
    case Set(sq, n, from) => {
      // Fill in this square
      state = SquareState.getFinishedState(n)
      // Tell all (other) parts that I belong to that I am now filled in
      groups.filter(_ != from).foreach(_ ! Set(this, n, this))
    }
    case Not(sq, n, from) => {
      // Update state
      state = state.not(n)
      if (finished)
        // Tell all parts that I belong to that I am now filled in
        groups.foreach(_ ! Set(this, value, this))
      else
        // Tell all (other) parts that I belong to that I cannot contain this value
        groups.filter(_ != from).foreach(_ ! Not(this, n, this))
    }
  }
}

// A row, column or box in the puzzle
class PuzzleGroup(val p: Puzzle, val grouptype: String, val num: Int) extends PuzzlePart {
  override def toString = grouptype + " " + num

  // List of squares (and state for each) in this part of the puzzle
  var squares = new scala.collection.mutable.HashMap[PuzzleSquare, SquareState]()

  // True if all the squares in this part are complete
  def finished = squares.values.forall(_.finished)

  def handleMessage = {
    case Set(sq, n, from) if (squares.contains(sq)) => {
      set(sq, n, from)
      checkGroup
    }
    case Not(sq, n, from) if (squares.contains(sq)) => {
      // Update local state for the square
      squares(sq) = squares(sq).not(n)
      checkGroup
    }
  }

  // Fill in a square with a final value
  def set(sq: PuzzleSquare, n: Int, from: PuzzlePart) = {
    // Update local state for the square
    squares(sq) = SquareState.getFinishedState(n)
    // Tell the square that it's filled in
    if (!sq.eq(from))
      sq ! Set(sq, n, this)
    // Tell all the other squares in this group that they cannot have this value
    squares.filter(kv => !kv._1.eq(sq) && !kv._2.finished).foreach(kv => {
      val s = kv._1
      squares(s) = squares(s).not(n)
      s ! Not(s, n, this)
    })
  }

  // Check the squares in this group, looking for ones we can fill in
  def checkGroup: Unit = {
    // See if there are any unused values that could only be assigned to one square in the group
    var foundOnly = false
    for (n <- 1 to 9 if !squares.values.exists(_.value == n)) {
      // Get a list of this group's squares that might still be assigned this value
      val open = squares.filter(kv => !kv._2.finished && kv._2.isFlagOn(n)).toList
      open match {
        case (only, _) :: Nil => {
          // There's only one square, so fill it in
          foundOnly = true
          set(only, n, this)
        }
        case _ =>
      }
    }

    // TODO Do remaining squares that can be assigned a value have another group in common?

    // If this group is finished, tell the puzzle
    if (finished)
      p ! Finished(this)
    // If we found and filled in an "only", we should run checkGroup again
    else if (foundOnly)
      checkGroup
    else
      ()
  }
}

// One whole puzzle - singleton
class Puzzle extends PuzzlePart {
  // Create the groups
  val rows = (1 to 9).map(new PuzzleGroup(this, "Row", _)).toList
  val columns = (1 to 9).map(new PuzzleGroup(this, "Column", _)).toList
  val boxes = (1 to 9).map(new PuzzleGroup(this, "Box", _)).toList
  val allGroups = rows ::: columns ::: boxes
  var unfinishedGroups = allGroups

  // Get the box group for a given row & column
  def getBox(r:Int, c:Int) = boxes(3 * ((r - 1) / 3) + ((c - 1) / 3))

  // Create the squares
  val squares =
    for {
      row <- rows
      column <- columns
      box <- List(getBox(row.num, column.num))
    } yield new PuzzleSquare(row, column, box)
  val squaresArray = squares.toArray

  // Get the square for a given row & column
  def getSquare(r: Int, c: Int) = squaresArray(9 * (r - 1) + (c - 1))

  // Tell the groups about their squares
  allGroups.foreach(g =>
    squares.filter(_.groups.contains(g)).foreach(s => g.squares.put(s, s.state)))

  // Start everything!
  squares.foreach(_.start)
  allGroups.foreach(_.start)
  this.start

  // True if all the groups are finished
  def finished = unfinishedGroups.isEmpty

  def handleMessage = {
    case Put(r, c, n) => {
      val sq = getSquare(r, c)
      sq ! Set(sq, n, this)
    }
    case Load(s) =>
      load(s)
    case Finished(p: PuzzleGroup) =>
      unfinishedGroups -= p
    case _ =>
  }

  // Load the puzzle from a string with 81 digits
  // Q: Is there a better approach to this than using these two regex patterns?
  def load(s: String) = {
    // Make sure the input string has 81 digits
    val loadMatcher = """\s*(\d\s*){81}""".r.findFirstIn(s)
    loadMatcher match {
      case Some(digits) => {
        val digitMatcher = """\d""".r.findAllIn(digits)
        for {r <- 1 to 9
             c <- 1 to 9} {
          val n = digitMatcher.next.toInt
          if (n != 0)
            this ! Put(r, c, n)
        }
      }
      case None =>
    }
  }

  def printString = // Q: Can we do this? Is this accessing mutable state?
    (1 to 9).map(r => (1 to 9).map(c => getSquare(r, c).value).mkString).mkString("\n")

  override def toString = "Puzzle"
}
