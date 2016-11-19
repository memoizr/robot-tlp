package robot

import robot.RobotAt._
import shapeless._
import shapeless.nat._
import shapeless.ops.hlist.LeftFolder
import shapeless.ops.nat._

import scala.annotation.implicitNotFound

case class RobotAt[latitude <: Nat: isInsideWarehouse, longitude <: Nat: isInsideWarehouse]() {

  def move(towardsDirection: Direction)(
    implicit
    lookForA: ProofThatItCanMove[towardsDirection.type, latitude, longitude]
  ) = lookForA.validMove()

  def moveAlongRoute[head <: Direction, tail <: HList](route: head :: tail)(
    implicit
    moveAlong: FollowRoute[head :: tail, RobotAt[latitude, longitude], Router.type]
  ): moveAlong.Out = moveAlong(route, RobotAt[latitude, longitude]())
}

object RobotAt {
  @implicitNotFound(
    """Illegal route: cannot move along route=${route} from position=${initialPosition} when using Router=${router}.
Please ensure robot stays within the confines of the warehouse (_0 <= position <= _9)."""
  )
  type FollowRoute[route <: HList, initialPosition, router] = LeftFolder[route, initialPosition, router]

  @implicitNotFound("Illegal position: ${position} is not <= _9")
  type isInsideWarehouse[position <: Nat] = position LTEq _9
}

object Warehouse {
  def withRobotAt[latitude <: Nat: isInsideWarehouse, longitude <: Nat: isInsideWarehouse]() = RobotAt[latitude, longitude]()
}
