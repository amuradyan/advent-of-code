val projectPath = new java.io.File(".").getCanonicalPath

// Part 1

val (depth, worngDistance) = scala.io.Source
   .fromFile(projectPath + "/puzzles/d2/input")
   .getLines
   .toList
   .map { case s"$direction $magnitude" => (direction, magnitude.toInt) }
   .foldLeft((0, 0)) { (totalDepthAndDistance, command) =>
      command match {
         case ("forward", value) =>
            (totalDepthAndDistance._1, totalDepthAndDistance._2 + value)
         case ("up", value) =>
            (totalDepthAndDistance._1 - value, totalDepthAndDistance._2)
         case ("down", value) =>
            (totalDepthAndDistance._1 + value, totalDepthAndDistance._2)
      }
   }

depth * worngDistance

// Part 2

case class Telemetry(aim: Int, depth: Int, distance: Int)

val telemetry = scala.io.Source
   .fromFile(projectPath + "/puzzles/d2/input")
   .getLines
   .toList
   .map { case s"$direction $magnitude" => (direction, magnitude.toInt) }
   .toList
   .foldLeft(Telemetry(0, 0, 0)) { (telemetry, command) =>
      command match {
         case ("forward", value) =>
            Telemetry(
              telemetry.aim,
              telemetry.depth + value * telemetry.aim,
              telemetry.distance + value
            )
         case ("up", value) =>
            Telemetry(
              telemetry.aim - value,
              telemetry.depth,
              telemetry.distance
            )
         case ("down", value) =>
            Telemetry(
              telemetry.aim + value,
              telemetry.depth,
              telemetry.distance
            )
      }
   }

telemetry.depth * telemetry.distance
