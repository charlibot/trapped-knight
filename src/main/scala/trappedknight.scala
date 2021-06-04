case class Cell(i: Int, j: Int)

// can be simplified: https://github.com/buddhabrot/UlamAnalysis/blob/main/README.md
// https://medium.com/swlh/the-trapped-knight-revisited-132c034a8a28
def valueAt(cell: Cell): Int = 
  val d = Math.max(Math.abs(cell.i), Math.abs(cell.j))
  val sideLength = 2 * d + 1
  // Size: d=0 => 1, d=1 => 3, d=2 => 5, d=3 => 7, d=k => 2k+1
  val startCell = Cell(d, d - d*2 + 1)
  val startValue = 4 * d * d - 4 * d + 2
  cell match
    case `startCell` => startValue
    case Cell(i, j) if i == d && j > -d => startValue + (d + j) - 1
    case Cell(i, j) if j == d => startValue + (sideLength - 2) + (d - i)
    case Cell(i, j) if i == -d => startValue + (sideLength - 2) + (sideLength - 1) + (d - j)
    case Cell(i, j) if j == -d => startValue + (sideLength - 2) + (sideLength - 1) * 2 + (d + i) 

import cats.implicits._
import cats.Applicative
// Would be nice: cartesian product with filter when same abs value
// monoid the cell with +. 
def possiblePositions(position: Cell): Set[Cell] = 
  val x = List(2, 1, -2, -1)
  Applicative[List]
    .product(x, x)
    .filterNot((i, j) => Math.abs(i) == Math.abs(j))
    .map((x, y) => Cell(position.i + x, position.j + y))
    .toSet
  // Set(
  //   Cell(position.i + 2, position.j + 1),
  //   Cell(position.i + 2, position.j - 1),
  //   Cell(position.i - 2, position.j + 1),
  //   Cell(position.i - 2, position.j - 1),
  //   Cell(position.i + 1, position.j + 2),
  //   Cell(position.i + 1, position.j - 2),
  //   Cell(position.i - 1, position.j + 2),
  //   Cell(position.i - 1, position.j - 2),
  // )

def determineEnd(current: Cell, seen: Set[Cell]): Int = 
  val p = possiblePositions(current)
  // find the Cells that have not been seen
  val x = p diff seen
  if (x.isEmpty) valueAt(current)
  else determineEnd(x.map(c => (valueAt(c), c)).toList.sortBy(_._1).head._2, seen + current)

@main
def main =
  println(determineEnd(Cell(0, 0), Set()))

