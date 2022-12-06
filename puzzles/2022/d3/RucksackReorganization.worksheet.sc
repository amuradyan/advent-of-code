val projectPath = new java.io.File(".").getCanonicalPath

val rawItems = scala.io.Source
   .fromFile(projectPath + "/puzzles/2022/d3/input")
   .getLines
   .toList


def evaluateItem(item: Char) = item match
   case lowercase if 'a' to 'z' contains lowercase => lowercase - 96
   case uppercase if 'A' to 'Z' contains uppercase => uppercase - 38

// Part 1: The duplicate

rawItems
   .map(items => items.splitAt(items.length() / 2))
   .map { case (compartmentOne, compartmentTwo) =>
      compartmentOne.toSet intersect compartmentTwo.toSet
   }
   .flatten
   .map(evaluateItem)
   .sum

// Part 2: Fresh badges

rawItems
   .map(_.toSet)
   .sliding(3, 3)
   .toList
   .map { 
      case List(compartmentOne, compartmentTwo, compartmentThree) =>
         compartmentOne intersect compartmentTwo intersect compartmentThree
      case _ => Set.empty
   }
   .flatten
   .map(evaluateItem)
   .sum