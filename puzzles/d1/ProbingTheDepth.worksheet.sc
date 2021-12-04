import scala.io.Source

val inclines = for {
  diffs <- Source
    .fromFile("/home/spectrum/playgraund/advent-of-code/puzzles/d1/data")
    .getLines
    .map(_.toInt)
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
