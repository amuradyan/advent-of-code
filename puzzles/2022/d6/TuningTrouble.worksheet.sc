val projectPath = new java.io.File(".").getCanonicalPath

val theSignal = scala.io.Source
   .fromFile(projectPath + "/puzzles/2022/d6/input")
   .getLines
   .toList
   .head

def markersFirstPosition(data: String, markerSize: Int) =
   data
      .sliding(markerSize)
      .toList
      .map(_.toSet)
      .zipWithIndex
      .filter(_._1.size == markerSize)
      .head
      ._2 + markerSize

val startOfAPacketMarkerSize = 4
val startOfAMessageMarkerSize = 14

// Part 1: First start-of-packet marker
markersFirstPosition(theSignal, startOfAPacketMarkerSize)

// Part 2: First start-of-a-message marker
markersFirstPosition(theSignal, startOfAMessageMarkerSize)
