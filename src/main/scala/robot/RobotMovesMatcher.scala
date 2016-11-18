package robot

import robot.RobotAt.isInsideWarehouse
import shapeless.ops.nat.Pred
import shapeless.{Nat, Poly, Succ}
import Direction._

object RobotMovesMatcher extends Poly {
  type `-1 =>`[input <: Nat, output <: Nat] = Pred.Aux[input, output]

  implicit def `can move SOUTH when`[longitude <: Nat, latitude <: Nat]
  (implicit
   `new southerly latitude is valid`: isInsideWarehouse[Succ[latitude]],
   `latitude is valid`: isInsideWarehouse[longitude]
  ) = use((robot: RobotAt[longitude, latitude], alongDirection: South) => robot.move(alongDirection))

  implicit def `can move EAST when`[longitude <: Nat, latitude <: Nat]
  (implicit
   `latitude is valid`: isInsideWarehouse[latitude],
   `new eastern latitude is valid`: isInsideWarehouse[Succ[longitude]]
  ) = use((robot: RobotAt[longitude, latitude], alongDirection: East) => robot.move(alongDirection))

  implicit def `can move NORTH when`[longitude <: Nat, latitude <: Nat, newLatitude <: Nat]
  (implicit
   `longitude is valid`: isInsideWarehouse[longitude],
   `latitude can be decreased`: latitude `-1 =>` newLatitude,
   `new latitude is valid`: isInsideWarehouse[newLatitude]
  ) = use((robot: RobotAt[longitude, latitude], alongDirection: North) => robot.move(alongDirection))

  implicit def `can move WEST when`[longitude <: Nat, latitude <: Nat, newLongitude <: Nat]
  (implicit
   `latitude is valid`: isInsideWarehouse[latitude],
   `longitude can be decreased`: longitude `-1 =>` newLongitude,
   `next western longitude is valid`: isInsideWarehouse[newLongitude]
  ) = use((robot: RobotAt[longitude, latitude], alongDirection: West) => robot.move(alongDirection))
}
