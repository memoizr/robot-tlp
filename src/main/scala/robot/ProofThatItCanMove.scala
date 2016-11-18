package robot

import robot.Direction.{East, North, South, West}
import robot.RobotAt.isInsideWarehouse
import shapeless.ops.nat.Pred
import shapeless.{Nat, Succ}

trait ProofThatItCanMove[direction <: Direction, longitude <: Nat, latitude <: Nat] {
  type NextPosition

  def validMove(): NextPosition
}

object ProofThatItCanMove {
  type `-1 =>`[input <: Nat, output <: Nat] = Pred.Aux[input, output]
  type Position[direction <: Direction, longitude <: Nat, latitude <: Nat, out] = ProofThatItCanMove[direction, longitude, latitude] {type NextPosition = out}

  implicit def `towards EAST if`[longitude <: Nat, latitude <: Nat]
  (
    implicit
    `next eastern longitude is valid`: isInsideWarehouse[Succ[longitude]],
    `latitude is valid`: isInsideWarehouse[latitude]
  ) = new ProofThatItCanMove[East, longitude, latitude] {
    type NextPosition = RobotAt[Succ[longitude], latitude]

    def validMove(): NextPosition = RobotAt[Succ[longitude], latitude]()
  }

  implicit def `towards SOUTH if`[longitude <: Nat, latitude <: Nat]
  (
    implicit
    `longitude is valid`: isInsideWarehouse[longitude],
    `next southerly latitude is valid`: isInsideWarehouse[Succ[latitude]]
  ) = new ProofThatItCanMove[South, longitude, latitude] {
    type NextPosition = RobotAt[longitude, Succ[latitude]]

    def validMove(): NextPosition = RobotAt[longitude, Succ[latitude]]()
  }

  implicit def `towards NORTH if`[longitude <: Nat, latitude <: Nat, nextLatitude <: Nat]
  (
    implicit
    `longitude is valid`: isInsideWarehouse[longitude],
    `latitude can be decreased`: latitude `-1 =>` nextLatitude,
    `next northern latitude is valid`: isInsideWarehouse[nextLatitude]
  ) = new ProofThatItCanMove[North, longitude, latitude] {
    type NextPosition = RobotAt[longitude, nextLatitude]

    def validMove(): NextPosition = RobotAt[longitude, nextLatitude]()
  }

  implicit def `towards WEST if`[longitude <: Nat, latitude <: Nat, nextLongitude <: Nat]
  (
    implicit
    `latitude is valid`: isInsideWarehouse[latitude],
    `longitude can be decreased`: longitude `-1 =>` nextLongitude,
    `next western longitude is valid`: isInsideWarehouse[nextLongitude]
  ) = new ProofThatItCanMove[West, longitude, latitude] {
    type NextPosition = RobotAt[nextLongitude, latitude]

    def validMove(): NextPosition = RobotAt[nextLongitude, latitude]()
  }
}