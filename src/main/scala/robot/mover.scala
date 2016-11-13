package robot

import robot.PositionInsideTenByTenGrid.isWithinRange
import shapeless.ops.nat.Pred
import shapeless.{Nat, Poly, Succ}
import Direction._

object mover extends Poly {
  type Position[column <: Nat, row <: Nat] = PositionInsideTenByTenGrid[column, row]

  implicit def toSouth[column <: Nat, row <: Nat](implicit
                                                  rowValid: isWithinRange[Succ[row]],
                                                  columnValid: isWithinRange[column]
                                                 ) = use((x: Position[column, row], dir: South) => x.move(dir))

  implicit def toEast[column <: Nat, row <: Nat](implicit
                                                 rowValid: isWithinRange[row],
                                                 columnValid: isWithinRange[Succ[column]]
                                                ) = use((x: Position[column, row], dir: East) => x.move(dir))

  implicit def toNorth[column <: Nat, row <: Nat, nextRow <: Nat](implicit
                                                                  columnValid: isWithinRange[column],
                                                                  nextRow: Pred.Aux[row, nextRow],
                                                                  rowValid: isWithinRange[nextRow]
                                                                 ) = use((x: Position[column, row], dir: North) => x.move(dir))

  implicit def toWest[column <: Nat, row <: Nat, nextColumn <: Nat](implicit
                                                                    rowValid: isWithinRange[row],
                                                                    nextColumn: Pred.Aux[column, nextColumn],
                                                                    column: isWithinRange[nextColumn]
                                                                   ) = use((x: Position[column, row], dir: West) => x.move(dir))
}
