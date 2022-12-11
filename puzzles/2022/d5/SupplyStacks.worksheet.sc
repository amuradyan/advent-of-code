import collection.mutable.Stack

val projectPath = new java.io.File(".").getCanonicalPath

val rawSections = scala.io.Source
   .fromFile(projectPath + "/puzzles/2022/d5/input")
   .getLines
   .toList

val supplyStackMap = rawSections
   .takeWhile(_ != "")
   .reverse
   .drop(1)
   .map { row =>
      row
         .sliding(4, 4)
         .toList
         .map(_.trim)
         .map(_.replace("[", "").replace("]", ""))
         .zip(LazyList from 1)
         .filter(_._1.nonEmpty)
   }
   .flatten
   .foldLeft(Map[Int, List[String]]()) { case (acc, (crate, index)) =>
      acc.get(index) match
         case Some(stack) => acc + (index -> (crate :: stack))
         case None        => acc + (index -> List(crate))
   }

case class Move(from: Int, to: Int, count: Int)

val moves = rawSections
   .dropWhile(_ != "")
   .drop(1)
   .map { case s"move $count from $from to $to" =>
      Move(from.toInt, to.toInt, count.toInt)
   }

def reorganize(supplyStacks: Map[Int, List[String]], moves: List[Move]): Map[Int, List[String]] =
  moves
    .foldLeft(supplyStacks) { case (acc, Move(from, to, count)) =>
      val (fromStack, toStack) = (acc(from), acc(to))
      // As we are told, the CrateMover 9001 is notable for many new and exciting features:
      // - air conditioning
      // - leather seats
      // - an extra cup holder
      // - the ability to pick up and move multiple crates at once
      //
      // Add `.reverse` to `fromStack.take(count)` to get the Part1 solution
      val (newFromStack, newToStack) = (fromStack.drop(count), fromStack.take(count) ++ toStack)
      println(s"$acc | f:$from t:$to c:$count")
      acc + (from -> newFromStack) + (to -> newToStack)
    }

val reorganizedStack = reorganize(supplyStackMap, moves)

reorganizedStack
  .map((k, v) => (k, v.take(1)))
  .toList
  .sortBy(_._1)
  .map(_._2)
  .flatten
  .mkString
