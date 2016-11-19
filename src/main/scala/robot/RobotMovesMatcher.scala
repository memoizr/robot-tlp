package robot

import robot.RobotAt.isInsideWarehouse
import shapeless.ops.nat.Pred
import shapeless.{Nat, Poly, Succ}
import Direction._

object RobotMovesMatcher extends Poly {
  type `-1 =>`[input <: Nat, output <: Nat] = Pred.Aux[input, output]

  implicit def `can move NORTH when`[latitude <: Nat, longitude <: Nat](
    implicit
    `new northern latitude is valid`: isInsideWarehouse[Succ[latitude]],
    `latitude is valid`: isInsideWarehouse[longitude]
  ) = use((robot: RobotAt[latitude, longitude], alongDirection: North) => robot.move(alongDirection))

  implicit def `can move EAST when`[latitude <: Nat, longitude <: Nat](
    implicit
    `latitude is valid`: isInsideWarehouse[latitude],
    `new eastern latitude is valid`: isInsideWarehouse[Succ[longitude]]
  ) = use((robot: RobotAt[latitude, longitude], alongDirection: East) => robot.move(alongDirection))

  implicit def `can move SOUTH when`[latitude <: Nat, longitude <: Nat, newLatitude <: Nat](
    implicit
    `longitude is valid`: isInsideWarehouse[longitude],
    `latitude can be decreased`: latitude `-1 =>` newLatitude,
    `new southerly latitude is valid`: isInsideWarehouse[newLatitude]
  ) = use((robot: RobotAt[latitude, longitude], alongDirection: South) => robot.move(alongDirection))

  implicit def `can move WEST when`[latitude <: Nat, longitude <: Nat, newLongitude <: Nat](
    implicit
    `latitude is valid`: isInsideWarehouse[latitude],
    `longitude can be decreased`: longitude `-1 =>` newLongitude,
    `next western longitude is valid`: isInsideWarehouse[newLongitude]
  ) = use((robot: RobotAt[latitude, longitude], alongDirection: West) => robot.move(alongDirection))
}
