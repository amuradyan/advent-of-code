val projectPath = new java.io.File(".").getCanonicalPath

val gameLogs =
  scala.io.Source
  .fromFile(projectPath + "/puzzles/2023/d2/input")
  .getLines
  .toList

trait Color
object Red extends Color
object Blue extends Color
object Green extends Color

case class ColoredCube[C <: Color](color: C)

case class CubeSample[C <: Color](quantity: Int = 0, cube: ColoredCube[C])
type CubeSamples[C <: Color] = List[CubeSample[C]]

type RedSample = CubeSample[Red.type]
type BlueSample = CubeSample[Blue.type]
type GreenSample = CubeSample[Green.type]

case class CubeSet(
  reds: RedSample,
  blues: BlueSample,
  greens: GreenSample
)

val emptyCubeSet = CubeSet(
  reds = CubeSample(0, ColoredCube(Red)),
  blues = CubeSample(0, ColoredCube(Blue)),
  greens = CubeSample(0, ColoredCube(Green))
)

type CubeSets = List[CubeSet]

case class Game(id: Int, cubeSets: CubeSets)

def cubeSets(rawCubeSetSample: String): CubeSet =
  rawCubeSetSample
    .split(", ")
    .collect(cubeSamples)
    .foldLeft(emptyCubeSet) { (acc, sample) =>
      sample.cube.color match
        case Red =>
          acc.copy(reds = sample.asInstanceOf[RedSample])
        case Green =>
          acc.copy(greens = sample.asInstanceOf[GreenSample])
        case Blue =>
          acc.copy(blues = sample.asInstanceOf[BlueSample])
    }

val cubeSamples: PartialFunction[String, CubeSample[Color]] = {
  case s"$quantity red" => CubeSample(quantity.toInt, ColoredCube(Red))
  case s"$quantity green" => CubeSample(quantity.toInt, ColoredCube(Green))
  case s"$quantity blue" => CubeSample(quantity.toInt, ColoredCube(Blue))
}

def validGame(game: Game, constraint: Constraint) =
  game.cubeSets.forall { set =>
    set.reds.quantity   < constraint.reds &&
    set.greens.quantity < constraint.greens &&
    set.blues.quantity  < constraint.blues
  }

def parseGame(rawGameSamples: String): CubeSets =
  rawGameSamples
    .split("; ")
    .collect(cubeSets(_))
    .toList

val games = gameLogs.collect {
  case s"Game $id: $gameSamples" => Game(id.toInt, parseGame(gameSamples))
}

// Part 1: Valid game sums

case class Constraint(
  reds: Int,
  blues: Int,
  greens: Int
)

object FirstConstraint extends Constraint(reds = 13, greens = 14, blues = 15)

games
  .filter(validGame(_, FirstConstraint))
  .map(_.id)
  .sum

// Part 2: Minimal power set sums

case class MinimalSetForGame(id: Int, minimalCubeSet: CubeSet)

def minimalCubeSet(game: Game): MinimalSetForGame = {
  val minimalCubeSet =
    game
      .cubeSets
      .foldLeft(emptyCubeSet) { (acc, sample) =>
        val newMaxRed = Math.max(acc.reds.quantity, sample.reds.quantity)
        val newMaxBlue = Math.max(acc.blues.quantity, sample.blues.quantity)
        val newMaxGreen = Math.max(acc.greens.quantity, sample.greens.quantity)

        emptyCubeSet.copy(
          reds = CubeSample(newMaxRed, ColoredCube(Red)),
          blues = CubeSample(newMaxBlue, ColoredCube(Blue)),
          greens = CubeSample(newMaxGreen, ColoredCube(Green))
        )
      }

  MinimalSetForGame(game.id, minimalCubeSet)
}

def powerOfTheMinimalSet(minimalSetForGame: MinimalSetForGame): Int =
  minimalSetForGame.minimalCubeSet.reds.quantity *
  minimalSetForGame.minimalCubeSet.blues.quantity *
  minimalSetForGame.minimalCubeSet.greens.quantity

games
  .map(minimalCubeSet)
  .map(powerOfTheMinimalSet)
  .sum
