package robot

import org.scalatest.{FlatSpec, Matchers}
import shapeless.Nat._
import shapeless.ops.nat.LTEq
import shapeless.test.illTyped
import shapeless.{Nat, Poly1}

import scala.language.higherKinds

trait isThereAValidGrid[column <: Nat, row <: Nat]

object isThereAValidGrid {
}

object validator extends Poly1 {
  import TenByTenGrid._
  implicit def parameter[n <: Nat : lessThanOrEqualTo9](implicit isThereAValidGrid: TenByTenGrid[n, n])  = at[n] {x => x}
}

class RobotTest extends FlatSpec with Matchers {
  it should "at most be ten by ten" in {
    val gridMin: TenByTenGrid[_0, _0] = TenByTenGrid.withRobotAt[_0, _0]() // compiles
    val gridMax: TenByTenGrid[_9, _9] = TenByTenGrid.withRobotAt[_9, _9]() // compiles

    illTyped {
      "val gridMin: TenByTenGrid[_10, _0] = TenByTenGrid.withRobotAt[_10, _0]"
    }
    illTyped {
      "val gridMax: TenByTenGrid[_0, _10] = TenByTenGrid.withRobotAt[_0, _10]"
    }
    illTyped {
      "val gridMax: TenByTenGrid[_10, _10] = TenByTenGrid.withRobotAt[_10, _10]"
    }
  }

  it should "move to a legal position" in {
//    val newGridAfterHavingMovedToSouth: TenByTenGrid[_1, _0] = TenByTenGrid.withRobotAt[_0, _0]().move[East] //compiles
//    val newGridAfterHavingMovedToEast: TenByTenGrid[_0, _1] = TenByTenGrid.withRobotAt[_0, _0].move[South] //compiles
//    val newGridAfterHavingMovedToNorth = TenByTenGrid.withRobotAt[_0, _0].move[North] //not compiles
//    val newGridAfterHavingMovedToEast = TenByTenGrid.withRobotAt[_0, _0].move[West] //not compiles
  }
}

import robot.TenByTenGrid._

sealed trait Direction

object Direction {
  type South = South.type
  type East = East.type
}

case object South extends Direction
case object East extends Direction

trait moveTo[direction <: Direction, column <: Nat, row <: Nat] {
}

//object moveTo {
//  type Aux[direction <: Direction, column <: Nat, row <: Nat, out <: (column, row)] = moveTo[direction, column, row] { type Out = (column, row)}
//}
//
//
case class TenByTenGrid[column <: Nat : lessThanOrEqualTo9, row <: Nat : lessThanOrEqualTo9]() {
  def move[direction <: Direction](
//                                  implicit
//                                  canMoveTo: moveTo[direction, column, row, out]
                                  ) = TenByTenGrid[column, row]()
}


trait coordinates[Column <: Nat, Row <: Nat] {
  type column = Column
  type row = Row
}
object TenByTenGrid {
  type coordinates[column, row] = (column, row)
  type lessThanOrEqualTo9[position <: Nat] = position LTEq _9

  def withRobotAt[column <: Nat : lessThanOrEqualTo9, row <: Nat : lessThanOrEqualTo9]() = TenByTenGrid[column, row]()
}

