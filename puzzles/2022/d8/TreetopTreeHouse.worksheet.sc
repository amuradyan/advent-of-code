val projectPath = new java.io.File(".").getCanonicalPath

val gridMap = scala.io.Source
   .fromFile(projectPath + "/puzzles/2022/d8/input")
   .getLines
   .toList
   .map { n =>
      n.map(_.asDigit).toList
   }

val perimeter = 2 * (gridMap.length + gridMap(0).length) - 4

val visibilityMap = for
  i <- 1 to (gridMap(0).length - 2)
  j <- 1 to (gridMap.length - 2)
  current = gridMap(i)(j)
  isVisibleHorizontally = isVisible(j, gridMap(i))
  horizontalScenicScore = scenicScore(j, gridMap(i))
  transposed = gridMap.transpose
  isVisibleVertically = isVisible(i, transposed(j))
  verticalScenicScore = scenicScore(i, transposed(j))
yield isVisibleHorizontally || isVisibleVertically

val scenicScores = for
  i <- 1 to (gridMap(0).length - 2)
  j <- 1 to (gridMap.length - 2)
  current = gridMap(i)(j)
  horizontalScenicScore = scenicScore(j, gridMap(i))
  transposed = gridMap.transpose
  verticalScenicScore = scenicScore(i, transposed(j))
yield horizontalScenicScore * verticalScenicScore


extension (xs: List[Int])
  def takeWhileBlocked(threshold: Int): List[Int] = xs match
    case head :: next :: tail => {
      if head >= threshold then head :: Nil
      else head :: (next :: tail).takeWhileBlocked(threshold)
    }
    case head :: Nil => head :: Nil
    case Nil => Nil


def scenicScore(idx: Int, xs: List[Int]): Int =
  val rightScore = xs.drop(idx + 1).takeWhileBlocked(xs(idx)).length
  val leftScore = xs.take(idx).reverse.takeWhileBlocked(xs(idx)).length

  leftScore * rightScore


def isVisible(idx: Int, xs: List[Int]): Boolean = {
  val current = xs(idx)
  val left = xs.take(idx)
  val right = xs.drop(idx + 1)

  left.filter(_ >= current).isEmpty || right.filter(_ >= current).isEmpty
}

val innerTrees = visibilityMap
  .foldLeft(0) { (acc, x) =>
    if x then acc + 1 else acc
  }


innerTrees + perimeter

scenicScores.max
