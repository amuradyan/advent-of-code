import scala.util.Try
val projectPath = new java.io.File(".").getCanonicalPath

val historicallySignificantLocations =
   scala.io.Source
      .fromFile(projectPath + "/puzzles/2024/d1/input")
      .getLines
      .toList

// Part 1

val List(left, right) =
   historicallySignificantLocations
      .map(_.split("   ").map(_.toInt).toList)
      .transpose
      .map(_.sortWith(_ < _))

left.zip(right).map { case (l, r) => Math.abs(r - l) }.sum

// Part 2

left
   .map { number =>
      (number, right.count(_ == number))
   }
   .filter { case (_, count) => count != 0 }
   .map { case (number, count) => number * count }
   .sum
