val projectPath = new java.io.File(".").getCanonicalPath

val rawHeightmap = List("2199943210", "3987894921", "9856789892", "8767896789", "9899965678")

// val rawHeightmap = scala.io.Source
//    .fromFile(projectPath + "/puzzles/d9/input")
//    .getLines
//    .toList

type Heightmap = List[List[Int]]

rawHeightmap
   .map {
      _.map {
         _.toInt - 48
      }.toList
   }

val nums = List(1, 2, 3, 4, 5)
nums.filter(_ % 2 != 0)
val isOdd = (n: Int) => n % 2 != 0;
nums.filter(isOdd);

nums.fold(0) { (acc, n) =>
   acc + n
}
nums.foldLeft(0) { (acc, n) =>
   acc + n
}
nums.foldRight(0) { (n, acc) =>
   acc + n
}
