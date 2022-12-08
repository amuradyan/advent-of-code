val projectPath = new java.io.File(".").getCanonicalPath

val rawSections = scala.io.Source
   .fromFile(projectPath + "/puzzles/2022/d4/input")
   .getLines
   .toList

// Part 1: Full overlaps

rawSections
   .map { case s"$l1-$r1,$l2-$r2" =>
      ((l1.toInt to r1.toInt).toSet, (l2.toInt to r2.toInt).toSet)
   }
   .foldLeft(0) { case (acc, (l, r)) =>
      if r.subsetOf(l) || l.subsetOf(r) then acc + 1 else acc
   }

// Part 2: Partial overlaps

rawSections
   .map { case s"$l1-$r1,$l2-$r2" =>
      ((l1.toInt to r1.toInt).toSet, (l2.toInt to r2.toInt).toSet)
   }
   .foldLeft(0) { case (acc, (l, r)) =>
      if r.intersect(l).nonEmpty then acc + 1 else acc
   }