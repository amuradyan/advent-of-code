val projectPath = new java.io.File(".").getCanonicalPath

// Test data. The result should be:
//  - 37 for part one
//  - 168 for part two
//
// val crabHorizontalPositions = List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)

val crabHorizontalPositions = scala.io.Source
   .fromFile(projectPath + "/puzzles/d7/input")
   .getLines
   .map {
      _.split(",").map(_.toInt).toList
   }
   .flatten
   .toList

Range(1, crabHorizontalPositions.max)
   .foldLeft((0, Int.MaxValue)) { (acc, v) =>
      val fuelToll = crabHorizontalPositions.map { e =>
         // Math.abs(v - e)
         // NOTE: For Part 1, uncomment the above line and comment the 2 lines below.
         val distance = Math.abs(v - e)
         Range(0, distance + 1).sum
      }.sum

      if (fuelToll < acc._2) (v, fuelToll) else acc
   }
   ._2
