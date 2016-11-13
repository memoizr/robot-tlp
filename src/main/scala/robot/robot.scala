package robot
import robot.Direction._
import robot.PositionInsideTenByTenGrid.lessThanOrEqualTo9
import shapeless._
import shapeless.nat._
import shapeless.ops.nat._

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

    override def apply(): Out = PositionInsideTenByTenGrid[out, row]()
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
