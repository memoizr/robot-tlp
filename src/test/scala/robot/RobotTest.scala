package robot

import com.sun.org.apache.xpath.internal.operations.Gte
import org.scalatest.{FlatSpec, Matchers}
import robot.Direction.{East, North, South, West}
import robot.PositionInsideTenByTenGrid._
import shapeless.Nat._
import shapeless.ops.nat.{GTEq, LTEq, Pred}
import shapeless.test.illTyped
import shapeless.{Nat, Succ}

import scala.language.higherKinds

class RobotTest extends FlatSpec with Matchers {
  it should "at most be ten by ten" in {
    val gridMin: PositionInsideTenByTenGrid[_0, _0] = TenByTenGrid.withRobotAt[_0, _0]() // compiles
    val gridMax: PositionInsideTenByTenGrid[_9, _9] = TenByTenGrid.withRobotAt[_9, _9]() // compiles

    illTyped {
      "val gridMin = TenByTenGrid.withRobotAt[_10, _0]"
    }
    illTyped {
      "val gridMax = TenByTenGrid.withRobotAt[_0, _10]"
    }
    illTyped {
      "val gridMax = TenByTenGrid.withRobotAt[_10, _10]"
    }
  }

  it should "move to a legal position" in {
    val newGridAfterHavingMovedToSouth: PositionInsideTenByTenGrid[_1, _0] = TenByTenGrid.withRobotAt[_0, _0]().move(East) //compiles
    val newGridAfterHavingMovedToSouth2: PositionInsideTenByTenGrid[_9, _0] = TenByTenGrid.withRobotAt[_8, _0]().move(East) //compiles
    val newGridAfterHavingMovedToEast: PositionInsideTenByTenGrid[_0, _1] = TenByTenGrid.withRobotAt[_0, _0]().move(South) //compiles
    val newGridAfterHavingMovedToNorth: PositionInsideTenByTenGrid[_0, _8] = TenByTenGrid.withRobotAt[_0, _9]().move(North) //compiles
    val newGridAfterHavingMovedToNorth2: PositionInsideTenByTenGrid[_0, _0] = TenByTenGrid.withRobotAt[_0, _1]().move(North) //compiles
  }
}


sealed trait Direction

object Direction {
  type South = South.type
  type East = East.type
  type North = North.type
  type West = North.type
}

case object South extends Direction

case object East extends Direction

case object North extends Direction

case object West extends Direction

trait moveTo[direction <: Direction, x <: Nat, row <: Nat] {
  type Out

  def apply(): Out
}

object moveTo {
  type Aux[direction <: Direction, column <: Nat, row <: Nat, out] = moveTo[direction, column, row] {type Out = out}

  implicit def canMoveEast[column <: Nat, row <: Nat](implicit
                                                      isColumnInRange: lessThanOrEqualTo9[Succ[column]],
                                                      isRowInRange: lessThanOrEqualTo9[row]
                                                     ): moveTo.Aux[East, column, row, PositionInsideTenByTenGrid[Succ[column], row]] = new moveTo[East, column, row] {
    override type Out = PositionInsideTenByTenGrid[Succ[column], row]

    override def apply(): Out = PositionInsideTenByTenGrid[Succ[column], row]()
  }

  implicit def canMoveSouth[column <: Nat, row <: Nat](implicit
                                                       isColumnInRange: lessThanOrEqualTo9[column],
                                                       isRowInRange: lessThanOrEqualTo9[Succ[row]]
                                                      ): moveTo.Aux[South, column, row, PositionInsideTenByTenGrid[column, Succ[row]]] = new moveTo[South, column, row] {
    override type Out = PositionInsideTenByTenGrid[column, Succ[row]]

    override def apply(): Out = PositionInsideTenByTenGrid[column, Succ[row]]()
  }

  implicit def canMoveNorth[column <: Nat, row <: Nat, out <: Nat](implicit
                                                                   isColumnInRange: lessThanOrEqualTo9[column],
                                                                   pred: Pred.Aux[row, out],
                                                                   isRowInRange: lessThanOrEqualTo9[out]
                                                                  ): moveTo.Aux[North, column, row, PositionInsideTenByTenGrid[column, out]] = new moveTo[North, column, row] {
    override type Out = PositionInsideTenByTenGrid[column, out]

    override def apply(): Out = PositionInsideTenByTenGrid[column, out]()
  }

  implicit def canMoveWest[column <: Nat, row <: Nat, out <: Nat](implicit
                                                                  isRowInRange: lessThanOrEqualTo9[row],
                                                                  pred: Pred.Aux[column, out],
                                                                  isColumnInRange: lessThanOrEqualTo9[out]
                                                                 ): moveTo.Aux[West, column, row, PositionInsideTenByTenGrid[out, row]] = new moveTo[West, column, row] {
    override type Out = PositionInsideTenByTenGrid[out, row]

    override def apply(): PositionInsideTenByTenGrid[out, row] = PositionInsideTenByTenGrid[out, row]()
  }
}

case class PositionInsideTenByTenGrid[column <: Nat : lessThanOrEqualTo9, row <: Nat : lessThanOrEqualTo9]() {
  def move(direction: Direction)(implicit attemptTo: moveTo[direction.type, column, row]) = attemptTo()
}


object PositionInsideTenByTenGrid {
  type lessThanOrEqualTo9[position <: Nat] = position LTEq _9
}

object TenByTenGrid {
  def withRobotAt[column <: Nat : lessThanOrEqualTo9, row <: Nat : lessThanOrEqualTo9]() = PositionInsideTenByTenGrid[column, row]()
}

