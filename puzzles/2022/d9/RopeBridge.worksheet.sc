val rawCommands = scala.io.Source
   .fromFile("./puzzles/2022/d9/input")
   .getLines
   .toList

trait Command

case class Up(n: Int) extends Command
case class Down(n: Int) extends Command
case class Left(n: Int) extends Command
case class Right(n: Int) extends Command

val commands = rawCommands
   .map {
      case s"R $n" => Right(n.toInt)
      case s"L $n" => Left(n.toInt)
      case s"U $n" => Up(n.toInt)
      case s"D $n" => Down(n.toInt)
   }

case class Position(x: Int, y: Int):
  def isNextTo(other: Position): Boolean =
    Math.abs(y - other.y) < 2 && Math.abs(x - other.x) < 2

Position(0, 0).isNextTo(Position(0, 1))
Position(0, 0).isNextTo(Position(1, 1))
Position(0, 0).isNextTo(Position(0, 2))
Position(0, 0).isNextTo(Position(1, 2))
Position(0, 0).isNextTo(Position(2, 1))

case class Head(position: Position)

case class Tail(position: Position, allPositions: Set[Position])

case class Rope(head: Head, tail: Tail):
  def moveDown(n: Int): Rope = n match
    case 1 =>  {
      val newHead = Position(head.position.x, head.position.y - 1)

      if tail.position.isNextTo(newHead) then
        Rope(Head(newHead), tail)
      else
        val newTail = Position(newHead.x, newHead.y + 1)
        Rope(Head(newHead), Tail(newTail, tail.allPositions + newTail))
    }
    case n => moveDown(1).moveDown(n - 1)

  def moveUp(n: Int): Rope = n match
    case 1 =>  {
      val newHead = Position(head.position.x, head.position.y + 1)

      if tail.position.isNextTo(newHead) then
        Rope(Head(newHead), tail)
      else
        val newTail = Position(newHead.x, newHead.y - 1)
        Rope(Head(newHead), Tail(newTail, tail.allPositions + newTail))
    }
    case n => moveUp(1).moveUp(n - 1)

  def moveLeft(n: Int): Rope = n match
    case 1 =>  {
      val newHead = Position(head.position.x - 1, head.position.y)

      if tail.position.isNextTo(newHead) then
        Rope(Head(newHead), tail)
      else
        val newTail = Position(newHead.x + 1, newHead.y)
        Rope(Head(newHead), Tail(newTail, tail.allPositions + newTail))
    }
    case n => moveLeft(1).moveLeft(n - 1)

  def moveRight(n: Int): Rope = n match
    case 1 =>  {
      val newHead = Position(head.position.x + 1, head.position.y)

      if tail.position.isNextTo(newHead) then
        Rope(Head(newHead), tail)
      else
        val newTail = Position(newHead.x - 1, newHead.y)
        Rope(Head(newHead), Tail(newTail, tail.allPositions + newTail))
    }
    case n => moveRight(1).moveRight(n - 1)

val start = Position(0, 0)

val rope = Rope(Head(start), Tail(start, Set(start)))

commands
  .foldLeft(rope){(rope, command) => command match
      case Down(n) => rope.moveDown(n)
      case Left(n) => rope.moveLeft(n)
      case Right(n) => rope.moveRight(n)
      case Up(n) => rope.moveUp(n)
    }
  .tail
  .allPositions
  .size