val projectPath = new java.io.File(".").getCanonicalPath

val rawGames = scala.io.Source
   .fromFile(projectPath + "/puzzles/2022/d2/input")
   .getLines
   .toList

trait Shape(val value: Int)

object Shape:
  def apply(input: String): Shape = input match
    case "A" | "X" => Rock()
    case "B" | "Y" => Paper()
    case "C" | "Z" => Scissors()
  
case class Rock() extends Shape(1)
case class Paper() extends Shape(2)
case class Scissors() extends Shape(3)

case class Score(p1: Int, p2: Int)

// Part 1: An interrupted chat

case class PartOneGame(shape1: Shape, shape2: Shape):
  def resolve = (shape1, shape2) match
    case (Rock(), Paper()) => Score(0 + 1, 6 + 2)
    case (Rock(), Scissors()) => Score(6 + 1, 0 + 3)
    case (Paper(), Scissors()) => Score(0 + 2, 6 + 3)
    case (Paper(), Rock()) => Score(6 + 2, 0 + 1)
    case (Scissors(), Rock()) => Score(0 + 3, 6 + 1)
    case (Scissors(), Paper()) => Score(6 + 3, 0 + 2)
    case _ => Score(3 + shape1.value, 3 + shape2.value)

def parsePartOneGame(game: String): PartOneGame = game match
  case s"$s1 $s2" => PartOneGame(Shape(s1), Shape(s2))

rawGames
   .map(parsePartOneGame)
   .map(_.resolve)
   .map(_.p2)
   .sum

// Part 2: A full game

trait Outcome(value: Int)
  
object Outcome:
   def apply(input: String): Outcome = input match
     case "X" => Loss()
     case "Y" => Draw()
     case "Z" => Win()

case class Loss() extends Outcome(0)
case class Draw() extends Outcome(3)
case class Win() extends Outcome(6)

case class PartTwoGame(opponentsShape: Shape, outcome: Outcome):
  def resolve = (opponentsShape, outcome) match
    case (Rock(), Win()) => Score(0 + 1, 6 + 2)
    case (Rock(), Draw()) => Score(3 + 1, 3 + 1)
    case (Rock(), Loss()) => Score(6 + 1, 0 + 3)
    case (Paper(), Win()) => Score(0 + 2, 6 + 3)
    case (Paper(), Draw()) => Score(3 + 2, 3 + 2)
    case (Paper(), Loss()) => Score(6 + 2, 0 + 1)
    case (Scissors(), Win()) => Score(0 + 3, 6 + 1)
    case (Scissors(), Draw()) => Score(3 + 3, 3 + 3)
    case (Scissors(), Loss()) => Score(6 + 3, 0 + 2)

def parsePartTwoGame(game: String): PartTwoGame = game match
  case s"$shape $outcome" => PartTwoGame(Shape(shape), Outcome(outcome))
  case _ => throw new Exception("Invalid game")

rawGames
   .map(parsePartTwoGame)
   .map(_.resolve)
   .map(_.p2)
   .sum
