package robot

import robot.RobotAt.isInsideWarehouse
import shapeless._
import shapeless.nat._
import shapeless.ops.hlist.LeftFolder
import shapeless.ops.nat._

case class RobotAt[latitude <: Nat : isInsideWarehouse, longitude <: Nat : isInsideWarehouse]() {

  def move(towardsDirection: Direction)
          (implicit lookForA: ProofThatItCanMove[towardsDirection.type, latitude, longitude]) = lookForA.validMove()

  def moveRepeatedly[head <: Direction, tail <: HList]
  (listOfDirections: head :: tail)
  (
    implicit
    moveAlong: LeftFolder[head :: tail, RobotAt[latitude, longitude], RobotMovesMatcher.type]
  ): moveAlong.Out = moveAlong(listOfDirections, RobotAt[latitude, longitude]())
}

object RobotAt {
  type isInsideWarehouse[position <: Nat] = position LTEq _9
}

object Warehouse {
  def withRobotAt[latitude <: Nat : isInsideWarehouse, longitude <: Nat : isInsideWarehouse]() = RobotAt[latitude, longitude]()
}
