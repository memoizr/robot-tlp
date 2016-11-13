package robot

import robot.Direction._
import robot.PositionInsideTenByTenGrid.isWithinRange
import shapeless._
import shapeless.nat._
import shapeless.ops.nat._

trait moveTo[direction <: Direction, x <: Nat, row <: Nat] {
  type Out

  def apply(): Out
}

object moveTo {
  type Position[column <: Nat, row <: Nat] = PositionInsideTenByTenGrid[column, row]

  def Position[column <: Nat : isWithinRange, row <: Nat : isWithinRange] = PositionInsideTenByTenGrid[column, row]()

  type Aux[direction <: Direction, column <: Nat, row <: Nat, out] = moveTo[direction, column, row] {type Out = out}

  implicit def canMoveEast[column <: Nat, row <: Nat](implicit
                                                      isColumnInRange: isWithinRange[Succ[column]],
                                                      isRowInRange: isWithinRange[row]
                                                     ): moveTo.Aux[East, column, row, Position[Succ[column], row]] = new moveTo[East, column, row] {
    override type Out = Position[Succ[column], row]

    override def apply(): Out = Position[Succ[column], row]
  }

  implicit def canMoveSouth[column <: Nat, row <: Nat](implicit
                                                       isColumnInRange: isWithinRange[column],
                                                       isRowInRange: isWithinRange[Succ[row]]
                                                      ): moveTo.Aux[South, column, row, Position[column, Succ[row]]] = new moveTo[South, column, row] {
    override type Out = Position[column, Succ[row]]

    override def apply(): Out = Position[column, Succ[row]]
  }

  implicit def canMoveNorth[column <: Nat, row <: Nat, nextRow <: Nat](implicit
                                                                       isColumnInRange: isWithinRange[column],
                                                                       nextRow: Pred.Aux[row, nextRow],
                                                                       isRowInRange: isWithinRange[nextRow]
                                                                      ): moveTo.Aux[North, column, row, Position[column, nextRow]] = new moveTo[North, column, row] {
    override type Out = Position[column, nextRow]

    override def apply(): Out = Position[column, nextRow]
  }

  implicit def canMoveWest[column <: Nat, row <: Nat, nextColumn <: Nat](implicit
                                                                         isRowInRange: isWithinRange[row],
                                                                         nextColumn: Pred.Aux[column, nextColumn],
                                                                         isColumnInRange: isWithinRange[nextColumn]
                                                                        ): moveTo.Aux[West, column, row, Position[nextColumn, row]] = new moveTo[West, column, row] {
    override type Out = Position[nextColumn, row]

    override def apply(): Out = Position[nextColumn, row]
  }
}

case class PositionInsideTenByTenGrid[column <: Nat : isWithinRange, row <: Nat : isWithinRange]() {
  def move(direction: Direction)(implicit attemptTo: moveTo[direction.type, column, row]) = attemptTo()
}

object PositionInsideTenByTenGrid {
  type isWithinRange[position <: Nat] = position LTEq _9
}

object TenByTenGrid {
  def withRobotAt[column <: Nat : isWithinRange, row <: Nat : isWithinRange]() = PositionInsideTenByTenGrid[column, row]()
}
