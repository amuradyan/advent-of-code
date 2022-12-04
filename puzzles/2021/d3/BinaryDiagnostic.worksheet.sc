val projectPath = new java.io.File(".").getCanonicalPath

// Part 1 : Power Consumption

scala.io.Source
   .fromFile(projectPath + "/puzzles/2021/d3/input")
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

// Part 2 : Life Support Rating

//    For the values below the oxygen generator and CO2 scrubber ratings
//    should be 23 and 10 respectively.
//    The life support rating should be 230.
//
// val telemetry = List(
//   "00100",
//   "11110",
//   "10110",
//   "10111",
//   "10101",
//   "01111",
//   "00111",
//   "11100",
//   "10000",
//   "11001",
//   "00010",
//   "01010"
//   ).map(_.getBytes.toList.map { _ - 48 })

type Telemetry = List[List[Int]]
type TelemetryColumn = List[Int]

def findOxygenRatingBitCriteria(bits: TelemetryColumn): Int = {
   val bitBias = bits
      .map(bit => if (bit == 1) 1 else -1)
      .sum

   if (bitBias >= 0) 1 else 0
}

def findCO2ScrubberRatingBitCriteria(bits: TelemetryColumn): Int =
   Math.abs(1 - findOxygenRatingBitCriteria(bits))

def filterTelemetryIteratively(telemetry: Telemetry, bitCriteria: TelemetryColumn => Int, columnIndex: Int): Int =
   telemetry match {
      case v :: Nil => Integer.parseInt(v.mkString, 2)
      case _ => {
         val criteria = bitCriteria(telemetry.map(_(columnIndex)))
         val matches = telemetry.filter(_(columnIndex) == criteria)
         filterTelemetryIteratively(matches, bitCriteria, columnIndex + 1)
      }
   }

def calculateOxygenGeneratorRating(telemetry: Telemetry): Int = {
   filterTelemetryIteratively(telemetry, findOxygenRatingBitCriteria, 0)
}

def calculateCO2ScrubberRating(telemetry: Telemetry): Int = {
   filterTelemetryIteratively(telemetry, findCO2ScrubberRatingBitCriteria, 0)
}

def calculateLifeSupportRating(telemetry: Telemetry): Int = {
   val oxygenGeneratorRating = calculateOxygenGeneratorRating(telemetry);
   val CO2ScrubberRating = calculateCO2ScrubberRating(telemetry);

   oxygenGeneratorRating * CO2ScrubberRating
}

val telemetry = scala.io.Source
   .fromFile(projectPath + "/puzzles/2021/d3/input")
   .getLines
   .toList
   .map(_.getBytes.toList.map { _ - 48 })

calculateLifeSupportRating(telemetry)
