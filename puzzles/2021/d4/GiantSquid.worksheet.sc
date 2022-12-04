val projectPath = new java.io.File(".").getCanonicalPath

// Part 1 : Finding the first winning bingo ticket

// Test data. The result should be:
//  - 4512 for part one
//  - 1924 for part two
//
// val gameData = List(
//   "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
//   "",
//   "22 13 17 11  0",
//   " 8  2 23  4 24",
//   "21  9 14 16  7",
//   " 6 10  3 18  5",
//   " 1 12 20 15 19",
//   "",
//   " 3 15  0  2 22",
//   " 9 18 13 17  5",
//   "19  8  7 25 23",
//   "20 11 10 24  4",
//   "14 21 16 12  6",
//   "",
//   "14 21 17 24  4",
//   "10 16 15  9 19",
//   "18  8 23 26 20",
//   "22 11 13  6  5",
//   " 2  0 12  3  7"
// ).toList

val gameData = scala.io.Source
   .fromFile(projectPath + "/puzzles/2021/d4/input")
   .getLines
   .toList

val drawnNumbers = gameData.head.split(",").map(_.toInt).toList
val allBoards = gameData.tail
   .filter(_.nonEmpty)
   .map {
      _.split(" ")
         .filter(_.nonEmpty)
         .map(_.toInt)
         .toList
   }
   .grouped(5)
   .toList

type Board = List[List[Int]]
type Boards = List[List[List[Int]]]
type BoardRow = List[Int]

def updateTheBoards(boards: Boards, number: Int): Boards = boards.map(updateTheBoard(_, number))

def updateTheBoard(board: Board, number: Int): Board = board.map(updateTheRow(_, number))

def updateTheRow(row: BoardRow, number: Int): BoardRow = row.map(n => if (n == number) -1 else n)

def isWinningBoard(board: Board): Boolean = board.map(_.sum).contains(-5) || board.transpose.map(_.sum).contains(-5)

val (winningBoards, firstLuckyNumber) = drawnNumbers
   .foldLeft((allBoards, -1)) { (acc, n) =>
      {
         val (boards, firstLuckyNumber) = acc

         if (firstLuckyNumber < 0) {
            val updatedBoards = updateTheBoards(boards, n)

            updatedBoards.filter(isWinningBoard) match {
               case b :: Nil => (b :: Nil, n)
               case _        => (updatedBoards, firstLuckyNumber)
            }
         } else (boards, firstLuckyNumber)
      }
   }

val sumOfUnmarkedForTheFirstWinningTicket = winningBoards.flatten.flatten
   .map(Math.max(0, _))
   .sum

firstLuckyNumber * sumOfUnmarkedForTheFirstWinningTicket

// Part 2 : Finding the last winning bingo ticket

val (_, lastWinningBoard, lastLuckyNumber) = drawnNumbers
   .foldLeft((allBoards, Nil: Board, -1)) { (acc, n) =>
      {
         val (boardsAtHand, currentWinningBoard, lastLuckyNumber) = acc
         val updatedBoards = updateTheBoards(boardsAtHand, n)
         val roundWinners = updatedBoards.filter(isWinningBoard)

         if (roundWinners.nonEmpty) {
            (updatedBoards.diff(roundWinners), roundWinners.head, n)
         } else {
            (updatedBoards, currentWinningBoard, lastLuckyNumber)
         }
      }
   }

val sumOfUnmarked = lastWinningBoard.flatten.filter(_ != -1).sum

lastLuckyNumber * sumOfUnmarked
