package robot

import shapeless._
import org.scalatest.{FlatSpec, Matchers}
import shapeless.Nat._
import shapeless.test.illTyped

import scala.language.higherKinds


class RobotTest extends FlatSpec with Matchers {

  it should "at most be ten by ten" in {
    val gridMin: PositionInsideTenByTenGrid[_0, _0] = TenByTenGrid.withRobotAt[_0, _0]() // compiles
    val gridMax: PositionInsideTenByTenGrid[_9, _9] = TenByTenGrid.withRobotAt[_9, _9]() // compiles

    illTyped {
      "val gridMin = TenByTenGrid.withRobotAt[_10, _0]"
    }
    illTyped {
      "val gridMax = TenByTenGrid.withRobotAt[_0, _10]"
    }
    illTyped {
      "val gridMax = TenByTenGrid.withRobotAt[_10, _10]"
    }
  }

  it should "move East within Range" in {
    val aValidGrid: PositionInsideTenByTenGrid[_1, _0] = TenByTenGrid.withRobotAt[_0, _0]().move(East) //compiles
    val anotherValidGrid: PositionInsideTenByTenGrid[_9, _8] = TenByTenGrid.withRobotAt[_8, _8]().move(East) //compiles

    illTyped {
      "TenByTenGrid.withRobotAt[_9, _0]().move(East)"
    }
  }

  it should "move South within Range" in {
    val aValidGrid: PositionInsideTenByTenGrid[_0, _1] = TenByTenGrid.withRobotAt[_0, _0]().move(South) //compiles
    val anotherValidGrid: PositionInsideTenByTenGrid[_2, _2] = TenByTenGrid.withRobotAt[_2, _1]().move(South) //compiles

    illTyped {
      "TenByTenGrid.withRobotAt[_0, _9]().move(South)"
    }
  }

  it should "move North within Range" in {
    val aValidGrid: PositionInsideTenByTenGrid[_0, _8] = TenByTenGrid.withRobotAt[_0, _9]().move(North) //compiles
    val anotherValidGrid: PositionInsideTenByTenGrid[_7, _6] = TenByTenGrid.withRobotAt[_7, _7]().move(North) //compiles

    illTyped {
      "TenByTenGrid.withRobotAt[_0, _0]().move(North)"
    }
  }

  it should "move West within Range" in {
    val aValidGrid: PositionInsideTenByTenGrid[_1, _2] = TenByTenGrid.withRobotAt[_2, _2]().move(West) //compiles

    illTyped {
      "TenByTenGrid.withRobotAt[_0, _3]().move(West)"
    }
  }

  it should "move South several times" in {
    val out: PositionInsideTenByTenGrid[_5, _7] = (South :: South :: HNil).foldLeft(TenByTenGrid.withRobotAt[_5, _5]())(mover)

    illTyped {
      "(South :: South :: HNil).foldLeft(TenByTenGrid.withRobotAt[_5, _8]())(mover)"
    }
  }

  it should "move East several times" in {
    val out: PositionInsideTenByTenGrid[_7, _5] = (East :: East :: HNil).foldLeft(TenByTenGrid.withRobotAt[_5, _5]())(mover)

    illTyped {
      "(East :: East :: HNil).foldLeft(TenByTenGrid.withRobotAt[_8, _5]())(mover)"
    }
  }

  it should "move North several times" in {
    val out: PositionInsideTenByTenGrid[_5, _3] = (North :: North :: HNil).foldLeft(TenByTenGrid.withRobotAt[_5, _5]())(mover)

    illTyped {
      "(North :: North :: HNil).foldLeft(TenByTenGrid.withRobotAt[_5, _1]())(mover)"
    }
  }

  it should "move West several times" in {
    val out: PositionInsideTenByTenGrid[_3, _5] = (West :: West :: HNil).foldLeft(TenByTenGrid.withRobotAt[_5, _5]())(mover)

    illTyped {
      "(West :: West :: HNil).foldLeft(TenByTenGrid.withRobotAt[_1, _5]())(mover)"
    }
  }

  it should "move like Jagger!" in {
    val out: PositionInsideTenByTenGrid[_4, _4] = (West :: West :: North :: North :: South :: East :: HNil).foldLeft(TenByTenGrid.withRobotAt[_5, _5]())(mover)
  }
}

