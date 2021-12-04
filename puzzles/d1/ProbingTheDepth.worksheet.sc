import scala.io.Source

val inclines = for {
   diffs <- Source
      .fromFile("/home/spectrum/playgraund/advent-of-code/puzzles/d1/data")
      .getLines
      .map(_.toInt)
      // If you comment the two lines below, you'll get the solution to part 1 of this puzzle
      .sliding(3)
      .map(_.sum)
      .sliding(2)
      .map { case Seq(l, r) => r - l }
      .filter(_ > 0)
      .toList
} yield diffs

inclines.size

Source
   .fromFile("/home/spectrum/playgraund/advent-of-code/puzzles/d1/data")
   .getLines
   .map(_.toInt)
   // If you comment the two lines below, you'll get the solution to part 1 of this puzzle
   .sliding(3)
   .map(_.sum)
   .sliding(2)
   .foldLeft(0) { (inclines, readings) =>
      {
         readings match {
            case Seq(l: Int, r: Int) =>
               if (r > l) { inclines + 1 }
               else inclines
         }
      }
   }
