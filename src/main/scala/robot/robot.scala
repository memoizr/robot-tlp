package robot

import robot.RobotAt.isInsideWarehouse
import shapeless._
import shapeless.nat._
import shapeless.ops.hlist.LeftFolder
import shapeless.ops.nat._

case class RobotAt[longitude <: Nat : isInsideWarehouse, latitude <: Nat : isInsideWarehouse]() {

  def move(towardsDirection: Direction)
          (implicit lookForA: ProofThatItCanMove[towardsDirection.type, longitude, latitude]) = lookForA.validMove()

  def moveRepeatedly[head <: Direction, tail <: HList]
  (listOfDirections: head :: tail)
  (
    implicit
    moveAlong: LeftFolder[head :: tail, RobotAt[longitude, latitude], RobotMovesMatcher.type]
  ): moveAlong.Out = moveAlong(listOfDirections, RobotAt[longitude, latitude]())
}

object RobotAt {
  type isInsideWarehouse[position <: Nat] = position LTEq _9
}

object Warehouse {
  def withRobotAt[longitude <: Nat : isInsideWarehouse, latitude <: Nat : isInsideWarehouse]() = RobotAt[longitude, latitude]()
}
