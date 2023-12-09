import scala.util.Try
val projectPath = new java.io.File(".").getCanonicalPath

val calibrationDocument =
  scala.io.Source
  .fromFile(projectPath + "/puzzles/2023/d1/input")
  .getLines
  .toList

// Part 1: Sum of calibration values
val digits =
      ('0' to '9').toList


val sumOfAllCalibrationValue =
    calibrationDocument
      .foldLeft(0) { (sumOfAllCalibrationValue, line) =>
         val calibrationValue = for {
            firstDigit <- line.find(digits.contains(_))
            lastDigit <- line.findLast(digits.contains(_))
            calibrationValue <- Try { s"$firstDigit$lastDigit".toInt }.toOption
         } yield calibrationValue

         sumOfAllCalibrationValue + calibrationValue.getOrElse(0)
      }


// Part 2: numbers can also be spelled

class Digit(val digit: String, val spelling: String, val value: Int)

object Zero extends Digit("0", "zero", 0)
object One extends Digit("1", "one", 1)
object Two extends Digit("2", "two", 2)
object Three extends Digit("3", "three", 3)
object Four extends Digit("4", "four", 4)
object Five extends Digit("5", "five", 5)
object Six extends Digit("6", "six", 6)
object Seven extends Digit("7", "seven", 7)
object Eight extends Digit("8", "eight", 8)
object Nine extends Digit("9", "nine", 9)

val richDigits =
  List( Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine )

case class RawCalibrationValue(firstDigit: Digit, lastDigit: Digit):
  val toInt = firstDigit.value * 10 + lastDigit.value

assert(findACalibrationValue(richDigits, "1abc2", None) == Some(RawCalibrationValue(One, Two)))
assert(findACalibrationValue(richDigits, "pqr3stu8vwx", None) == Some(RawCalibrationValue(Three, Eight)))
assert(findACalibrationValue(richDigits, "a1b2c3d4e5f", None) == Some(RawCalibrationValue(One, Five)))
assert(findACalibrationValue(richDigits, "treb7uchet", None) == Some(RawCalibrationValue(Seven, Seven)))
assert(findACalibrationValue(richDigits, "trebuchet", None) == None)

def findACalibrationValue(
  digits: List[Digit],
  calibrationDocumentEntry: String,
  momentaryCalibrationValue: Option[RawCalibrationValue])
: Option[RawCalibrationValue] =
  if (calibrationDocumentEntry.isEmpty) {
    momentaryCalibrationValue
  } else {
    val newMomentaryCalibrationValue =
      digits
        .find(
          digit =>
            calibrationDocumentEntry.startsWith(digit.digit) ||
            calibrationDocumentEntry.startsWith(digit.spelling)
        )
        .fold (momentaryCalibrationValue) { digit =>
          momentaryCalibrationValue.fold(Some(RawCalibrationValue(digit, digit))) { momentaryCalibrationValue =>
            Some(RawCalibrationValue(momentaryCalibrationValue.firstDigit, digit))
          }
        }

    findACalibrationValue(
      digits,
      calibrationDocumentEntry.drop(1),
      newMomentaryCalibrationValue)
  }

val sumOfAllCalibrationValue2 =
  calibrationDocument
    .foldLeft(0) { (sumOfAllCalibrationValue, line) =>
      val calibrationValue = findACalibrationValue(richDigits, line, None).fold(0)(_.toInt)
      sumOfAllCalibrationValue + calibrationValue
    }
