import scala.util.Try
val projectPath = new java.io.File(".").getCanonicalPath

val reports =
   scala.io.Source
      .fromFile(projectPath + "/puzzles/2024/d2/input")
      .getLines
      .map(_.split(" ").toList.map(_.toInt))
      .toList

def checkLevelForSafety(tolerance: Int)(levels: List[Int]): Boolean =
   def loop(first: Int, rest: List[Int], trend: Int, isSafe: Boolean, tolerance: Int): Boolean =
      rest match
        case Nil => isSafe
        case second :: rest if math.abs(first - second) > 3 || math.abs(first - second) < 1 =>
          if tolerance < 1 then
            false
          else
            loop(second, rest, trend, isSafe, tolerance - 1)
        case second :: rest =>
            val localTrend = (first - second).sign

            if localTrend != trend then
              if tolerance < 1 then
                false
              else
                loop(first, rest, trend, isSafe, tolerance - 1)
            else
              loop(second, rest, trend, isSafe, tolerance)

   levels match
      case first :: second :: rest if math.abs(first - second) > 3 || math.abs(first - second) < 1 =>
        if tolerance < 1 then
          false
        else
          checkLevelForSafety(tolerance - 1)(second :: rest)
      case first :: second :: rest =>
        loop(second, rest, (first - second).sign, true, tolerance)
      case _ =>
        false


// Part 1

reports
   .map(checkLevelForSafety(0))
   .count(identity)

// Part 2: the Problem Dampener

reports
  .map(checkLevelForSafety(1))
  .count(identity)
