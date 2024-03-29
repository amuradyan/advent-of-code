val projectPath = new java.io.File(".").getCanonicalPath

scala.io.Source
   .fromFile(projectPath + "/puzzles/2021/d1/input")
   .getLines
   .map(_.toInt)
   // If you comment the two lines below, you'll get the solution to part 1 of this puzzle
   .sliding(3)
   .map(_.sum)
   .sliding(2)
   .foldLeft(0) { (inclines, readings) =>
      readings match {
         case Seq(l: Int, r: Int) =>
            if (r > l) { inclines + 1 }
            else inclines
      }
   }
