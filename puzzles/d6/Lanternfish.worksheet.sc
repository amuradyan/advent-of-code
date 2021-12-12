val projectPath = new java.io.File(".").getCanonicalPath

// Test data. The result should be:
//  - 26 and 5934 for 18 and 80 days respectively for part one
//  -
// Initial state: 3,4,3,1,2
// After  1 day:  2,3,2,0,1
// After  2 days: 1,2,1,6,0,8
// After  3 days: 0,1,0,5,6,7,8
// After  4 days: 6,0,6,4,5,6,7,8,8
// After  5 days: 5,6,5,3,4,5,6,7,7,8
// After  6 days: 4,5,4,2,3,4,5,6,6,7
// After  7 days: 3,4,3,1,2,3,4,5,5,6
// After  8 days: 2,3,2,0,1,2,3,4,4,5
// After  9 days: 1,2,1,6,0,1,2,3,3,4,8
// After 10 days: 0,1,0,5,6,0,1,2,2,3,7,8
// After 11 days: 6,0,6,4,5,6,0,1,1,2,6,7,8,8,8
// After 12 days: 5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8
// After 13 days: 4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8
// After 14 days: 3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8
// After 15 days: 2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7
// After 16 days: 1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8
// After 17 days: 0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8
// After 18 days: 6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8

// val schoolSample = List(3, 4, 3, 1, 2)

val schoolSample = scala.io.Source
   .fromFile(projectPath + "/puzzles/d6/input")
   .getLines
   .map {
      _.split(",").map(_.toInt).toList
   }
   .flatten
   .toList

def simulateSchool(state: List[Int], iterations: Int): List[Int] = {
   if (iterations > 0) {
      val newFish = state.count(_ == 0)
      val newState = state
         .map {
            case 0      => 6
            case v: Int => v - 1
         }
         .appendedAll(List.fill(newFish)(8))

      simulateSchool(newState, iterations - 1)
   } else state
}

simulateSchool(schoolSample, 18).size

Range(0, 80)
   .foldLeft(schoolSample) { (state, day) =>
      val newFish = state.count(_ == 0)

      state
         .map {
            case 0      => 6
            case v: Int => v - 1
         }
         .appendedAll(List.fill(newFish)(8))
   }
   .size
