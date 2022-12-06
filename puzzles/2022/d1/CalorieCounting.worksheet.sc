val projectPath = new java.io.File(".").getCanonicalPath

case class State(
    caloriesCarriedByAllElves: List[Int],
    caloriesCarriedByCurrentElf: Int):

    def read: List[Int] = caloriesCarriedByCurrentElf :: caloriesCarriedByAllElves

val emptyState = State(Nil, 0)

val caloriesCarriedByTheElves: List[Int] =
   scala.io.Source
      .fromFile(projectPath + "/puzzles/2022/d1/input")
      .getLines
      .foldLeft(emptyState) { (state, line) =>
         line match {
            case "" =>
               State(state.caloriesCarriedByCurrentElf :: state.caloriesCarriedByAllElves, 0)
            case calorie =>
               State(state.caloriesCarriedByAllElves, state.caloriesCarriedByCurrentElf + calorie.toInt)
         }
      }
      .read


// Part 1: Most calories carried by a single elf
caloriesCarriedByTheElves.max

// Part 2: Calories carried by the top three elves
caloriesCarriedByTheElves
.sortWith(_ > _)
.take(3)
.sum
