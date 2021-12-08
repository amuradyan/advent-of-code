val projectPath = new java.io.File(".").getCanonicalPath

// Part 1 : Power Consumption

scala.io.Source
   .fromFile(projectPath + "/puzzles/d3/input")
   .getLines
   .toList
   .map(_.getBytes.toList.map {
      case '1' => 1;
      case _   => -1
   })
   .transpose
   .map(_.sum)
   .map { e => if (e > 0) List(1, 0) else List(0, 1) }
   .transpose
   .map(_.mkString)
   .map(Integer.parseInt(_, 2))
   .product
