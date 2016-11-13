package robot

import org.scalatest.{FlatSpec, Matchers}
import robot.Direction.{East, South}
import shapeless.Nat._
import shapeless.ops.nat.{LTEq, ToInt}
import shapeless.test.illTyped
import shapeless.{DepFn0, DepFn1, DepFn2, Nat, Poly1, Succ}

import scala.language.higherKinds

trait isThereAValidGrid[column <: Nat, row <: Nat]

object isThereAValidGrid {
}

object validator extends Poly1 {

  import TenByTenGrid._

  implicit def parameter[n <: Nat : lessThanOrEqualTo9](implicit isThereAValidGrid: TenByTenGrid[n, n]) = at[n] { x => x }
}

import robot.TenByTenGrid._

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
    val newGridAfterHavingMovedToSouth: TenByTenGrid[_1, _0] = TenByTenGrid.withRobotAt[_0, _0]().move(East) //compiles
    val newGridAfterHavingMovedToEast: TenByTenGrid[_0, _1] = TenByTenGrid.withRobotAt[_0, _0]().move(South) //compiles
    //    val newGridAfterHavingMovedToNorth = TenByTenGrid.withRobotAt[_0, _0].move[North] //not compiles
    //    val newGridAfterHavingMovedToEast = TenByTenGrid.withRobotAt[_0, _0].move[West] //not compiles
  }
}


sealed trait Direction

object Direction {
  type South = South.type
  type East = East.type
}

case object South extends Direction

case object East extends Direction

trait moveTo[direction <: Direction, x <: Nat, row <: Nat] {
  type Out

  def apply(): Out
}

object moveTo {
  type Aux[direction <: Direction, column <: Nat, row <: Nat] = moveTo[direction, column, row] {
  }

  implicit def moveToAux[direction <: Direction, column <: Nat, row <: Nat : lessThanOrEqualTo9](
                                                                                                  implicit
                                                                                                  bar: lessThanOrEqualTo9[Succ[column]]
                                                                                                )
  : moveTo.Aux[East, column, row] {type Out = TenByTenGrid[Succ[column], row]} =
    new moveTo[East, column, row] {
      override type Out = TenByTenGrid[Succ[column], row]

      override def apply(): Out = {
        TenByTenGrid[Succ[column], row]()
      }
    }

  implicit def moveToAuxSouth[direction <: Direction, column <: Nat : lessThanOrEqualTo9, row <: Nat](
                                                                                                       implicit
                                                                                                       bar: lessThanOrEqualTo9[Succ[row]]
                                                                                                     )
  : moveTo.Aux[South, column, row] {type Out = TenByTenGrid[column, Succ[row]]} =
    new moveTo[South, column, row] {
      override type Out = TenByTenGrid[column, Succ[row]]

      override def apply(): Out = {
        TenByTenGrid[column, Succ[row]]()
      }
    }
}


trait Grid

case class TenByTenGrid[column1 <: Nat : lessThanOrEqualTo9, row <: Nat : lessThanOrEqualTo9]() extends Grid {
  def move(direction: Direction)(
    implicit
    canMoveTo: moveTo.Aux[direction.type, column1, row]
  ) = canMoveTo()
}


trait coordinates[Column <: Nat, Row <: Nat] {
  type column = Column
  type row = Row
}

//trait isValid[n <: Nat] {
//  type out <: Nat
//}
object TenByTenGrid {
  //  implicit def isValid[n<: Nat : lessThanOrEqualTo9] = new isValid[n] { type out = n}
  type coordinates[column, row] = (column, row)
  type foo[position <: Nat] = position LTEq _9
  type lessThanOrEqualTo9[position <: Nat] = position LTEq _9

  def withRobotAt[column <: Nat : lessThanOrEqualTo9, row <: Nat : lessThanOrEqualTo9]() = TenByTenGrid[column, row]()
}

