package robot

import robot.PositionInsideTenByTenGrid.lessThanOrEqualTo9
import shapeless.ops.nat.Pred
import shapeless.{Nat, Poly, Succ}
import Direction._

object mover extends Poly {
  implicit def toSouth[column <: Nat, row <: Nat](implicit
                                                  rowValid: lessThanOrEqualTo9[Succ[row]],
                                                  columnValid: lessThanOrEqualTo9[column]
                                                 )
  = use((x: PositionInsideTenByTenGrid[column, row], dir: South) => x.move(dir))

  implicit def toEast[column <: Nat, row <: Nat](implicit
                                                 rowValid: lessThanOrEqualTo9[row],
                                                 columnValid: lessThanOrEqualTo9[Succ[column]]
                                                )
  = use((x: PositionInsideTenByTenGrid[column, row], dir: East) => x.move(dir))

  implicit def toNorth[column <: Nat, row <: Nat, out <: Nat](implicit
                                                              columnValid: lessThanOrEqualTo9[column],
                                                              pred: Pred.Aux[row, out],
                                                              rowValid: lessThanOrEqualTo9[out]
                                                             )
  = use((x: PositionInsideTenByTenGrid[column, row], dir: North) => x.move(dir))

  implicit def toWest[column <: Nat, row <: Nat, out <: Nat](implicit
                                                             rowValid: lessThanOrEqualTo9[row],
                                                             pred: Pred.Aux[column, out],
                                                             column: lessThanOrEqualTo9[out]
                                                            )
  = use((x: PositionInsideTenByTenGrid[column, row], dir: West) => x.move(dir))
}
