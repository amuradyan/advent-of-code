val projectPath = new java.io.File(".").getCanonicalPath

type DisplayReadings = List[String]
type OutputValue = List[String]
type DecodedDigits = Map[String, Int]

case class Clues(displayReadings: DisplayReadings, outputValue: OutputValue)

// Test data. The result should be:
//  - 26 for the first part
//

val rawClues = List(
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
  "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
  "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
  "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
  "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
  "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
  "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
  "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
  "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
  "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
)

// val rawClues = scala.io.Source
//    .fromFile(projectPath + "/puzzles/d8/input")
//    .getLines
//    .toList

val clues = rawClues.map { case s"$rawDisplayReadings | $rawOutputValue" =>
   val displayReadings = rawDisplayReadings.split(" ").toList
   val outputValue = rawOutputValue.split(" ").toList
   Clues(displayReadings, outputValue)
}.toList

// Part 1: Finding digits with unique numberes of sequences

clues
   .map(_.outputValue)
   .flatten
   .filter { sequence =>
      sequence.length == 2 ||
      sequence.length == 3 ||
      sequence.length == 4 ||
      sequence.length == 7
   }
   .size

// Part 2: Decoding the numbers

def identify1478(readings: DisplayReadings): DecodedDigits =
   readings
      .foldLeft(Map[String, Int]()) { (acc, reading) =>
         reading.length match {
            case 2 => acc + (reading -> 1)
            case 3 => acc + (reading -> 7)
            case 4 => acc + (reading -> 4)
            case 7 => acc + (reading -> 8)
            case _ => acc
         }
      }

def identify235(readings: DisplayReadings): DecodedDigits = {
   readings.filter(_.length == 5)

   Map[String, Int]()
}

val displayReadings = clues.map(_.displayReadings)

def identifyDigits(displayReading: DisplayReadings): DecodedDigits = {
   identify1478(displayReading) ++ identify235(displayReading)
}

identifyDigits(displayReadings.head)

clues
   .map(_.displayReadings)
   .map(identify1478)

// def identify069(readings: DisplayReadings, digits1478: DecodedDigits): DecodedDigits = {
//    val eight = digits1478.filter(_._2 == 8)
//    val one = digits1478.filter(_._2 == 2)

//    val zero = readings
//       .filter(_.length == 6)
//       .foldLeft(Map[String, Int]()) { (acc, reading) =>
//          if (reading.diff(eight.keySet.head).length == 1) {
//             Map(reading -> 0)
//          } else {
//             acc
//          }
//       }

// }
// val
