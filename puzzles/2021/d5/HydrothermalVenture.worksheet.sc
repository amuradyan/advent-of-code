val projectPath = new java.io.File(".").getCanonicalPath

// Test data. The result should be:
//  - 5 for part one
//  - 12 for part two
//
// val radarReadings = List(
//   "0,9 -> 5,9",
//   "8,0 -> 0,8",
//   "9,4 -> 3,4",
//   "2,2 -> 2,1",
//   "7,0 -> 7,4",
//   "6,4 -> 2,0",
//   "0,9 -> 2,9",
//   "3,4 -> 1,4",
//   "0,0 -> 8,8",
//   "5,5 -> 8,2"
// )

val radarReadings = scala.io.Source
   .fromFile(projectPath + "/puzzles/2021/d5/input")
   .getLines
   .toList

case class Coordinate(val x: Int, val y: Int)
case class Point(val x: Int, val y: Int)
case class Line(points: List[Point])

radarReadings
   .map { case s"$x1,$y1 -> $x2,$y2" =>
      Line(List(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt)))
   }
   .map {
      _.points match {
         case List(Point(x1, y1), Point(x2, y2)) if (x1 == x2) =>
            for p <- y1 to y2 by (y2 - y1).sign yield (Point(x1, p), 1)
         case List(Point(x1, y1), Point(x2, y2)) if (y1 == y2) =>
            for p <- x1 to x2 by (x2 - x1).sign yield (Point(p, y1), 1)
         // Part 2: Uncomment the case below to find the most dangerous
         // sports on horizontals, verticals and diagonals
         //
         // case List(Point(x1, y1), Point(x2, y2)) if (Math.abs(x1 - x2) == Math.abs(y1 - y2)) =>
         //    val xs = for x <- x1 to x2 by (x2 - x1).sign yield x
         //    val ys = for y <- y1 to y2 by (y2 - y1).sign yield y

         //    for ((x, y) <- xs zip ys) yield (Point(x, y), 1)
         case _ => Nil
      }
   }
   .flatten
   .groupMapReduce(identity)(_ => 1) { _ + _ }
   .filter(_._2 > 1)
   .size
