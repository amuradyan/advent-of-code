val projectPath = new java.io.File(".").getCanonicalPath

case class Coordinate(val x: Int, val y: Int)
case class Point(val x: Int, val y: Int)
case class Line(points: List[Point])

// Test data. The result should be:
//  - 5 for part one
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
//    .map { case s"$x1,$y1 -> $x2,$y2" =>
//       Line(List(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt)))
//    }

val radarReadings = scala.io.Source
   .fromFile(projectPath + "/puzzles/d5/input")
   .getLines
   .toList
   .map { case s"$x1,$y1 -> $x2,$y2" =>
      Line(List(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt)))
   }

def isLineHorizontalOrVertical(line: Line): Boolean = line.points
   .map(p => List(p.x, p.y))
   .transpose
   .map { l =>
      l.filter(_ != l.head)
   }
   .contains(Nil)

val onlyVerticalOrHorizontalLines = radarReadings.filter(isLineHorizontalOrVertical)

onlyVerticalOrHorizontalLines
   .map {
      _.points match {
         case List(Point(x1, y1), Point(x2, y2)) if (x1 == x2) =>
            for p <- y1 to y2 by (y2 - y1).sign yield (Point(x1, p), 1)
         case List(Point(x1, y1), Point(x2, y2)) if (y1 == y2) =>
            for p <- x1 to x2 by (x2 - x1).sign yield (Point(p, y1), 1)
         case _ => Nil
      }
   }
   .flatten
   .groupMapReduce(identity)(_ => 1) { _ + _ }
   .filter(_._2 > 1)
   .size
