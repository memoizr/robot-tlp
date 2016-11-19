package robot

sealed trait Direction

object Direction {
  type South = South.type
  type East = East.type
  type North = North.type
  type West = West.type
}

case object South extends Direction
case object East extends Direction
case object North extends Direction
case object West extends Direction

