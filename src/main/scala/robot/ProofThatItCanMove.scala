package robot

import robot.Direction.{East, North, South, West}
import robot.RobotAt.isInsideWarehouse
import shapeless.ops.nat.Pred
import shapeless.{Nat, Succ}

trait ProofThatItCanMove[direction <: Direction, latitude <: Nat, longitude <: Nat] {
  type NextPosition

  def validMove(): NextPosition
}

object ProofThatItCanMove {
  type `-1 =>`[input <: Nat, output <: Nat] = Pred.Aux[input, output]
  type Position[direction <: Direction, latitude <: Nat, longitude <: Nat, out] = ProofThatItCanMove[direction, latitude, longitude] {type NextPosition = out}

  implicit def `towards EAST if`[latitude <: Nat, longitude <: Nat]
  (
    implicit
    `next eastern longitude is valid`: isInsideWarehouse[Succ[longitude]],
    `latitude is valid`: isInsideWarehouse[latitude]
  ) = new ProofThatItCanMove[East, latitude, longitude] {
    type NextPosition = RobotAt[latitude, Succ[longitude]]

    def validMove(): NextPosition = RobotAt[latitude, Succ[longitude]]()
  }

  implicit def `towards NORTH if`[latitude <: Nat, longitude <: Nat]
  (
    implicit
    `longitude is valid`: isInsideWarehouse[longitude],
    `next northern latitude is valid`: isInsideWarehouse[Succ[latitude]]
  ) = new ProofThatItCanMove[North, latitude, longitude] {
    type NextPosition = RobotAt[Succ[latitude], longitude]

    def validMove(): NextPosition = RobotAt[Succ[latitude], longitude]()
  }

  implicit def `towards SOUTH if`[latitude <: Nat, longitude <: Nat, nextLatitude <: Nat]
  (
    implicit
    `longitude is valid`: isInsideWarehouse[longitude],
    `latitude can be decreased`: latitude `-1 =>` nextLatitude,
    `next southerly latitude is valid`: isInsideWarehouse[nextLatitude]
  ) = new ProofThatItCanMove[South, latitude, longitude] {
    type NextPosition = RobotAt[nextLatitude, longitude]

    def validMove(): NextPosition = RobotAt[nextLatitude, longitude]()
  }

  implicit def `towards WEST if`[latitude <: Nat, longitude <: Nat, nextLongitude <: Nat]
  (
    implicit
    `latitude is valid`: isInsideWarehouse[latitude],
    `longitude can be decreased`: longitude `-1 =>` nextLongitude,
    `next western longitude is valid`: isInsideWarehouse[nextLongitude]
  ) = new ProofThatItCanMove[West, latitude, longitude] {
    type NextPosition = RobotAt[latitude, nextLongitude]

    def validMove(): NextPosition = RobotAt[latitude, nextLongitude]()
  }
}