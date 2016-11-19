package robot

import shapeless._
import org.scalatest.{FlatSpec, Matchers}
import shapeless.Nat._
import shapeless.test.illTyped

import scala.language.higherKinds


class RobotTest extends FlatSpec with Matchers {

  it should "at most be ten by ten" in {
    val gridMin: RobotAt[_0, _0] = Warehouse.withRobotAt[_0, _0]() // compiles
    val gridMax: RobotAt[_9, _9] = Warehouse.withRobotAt[_9, _9]() // compiles

    illTyped("Warehouse.withRobotAt[_10, _0]")
    illTyped("Warehouse.withRobotAt[_0, _10]")
    illTyped("Warehouse.withRobotAt[_10, _10]")
  }

  it should "move East within Range" in {
    val aValidGrid: RobotAt[_0, _1] = Warehouse.withRobotAt[_0, _0]().move(East) //compiles
    val anotherValidGrid: RobotAt[_8, _9] = Warehouse.withRobotAt[_8, _8]().move(East) //compiles

    illTyped("Warehouse.withRobotAt[_5, _9]().move(East)")
  }

  it should "move South within Range" in {
    val aValidGrid: RobotAt[_8, _0] = Warehouse.withRobotAt[_9, _0]().move(South) //compiles
    val anotherValidGrid: RobotAt[_1, _1] = Warehouse.withRobotAt[_2, _1]().move(South) //compiles

    illTyped("Warehouse.withRobotAt[_0, _5]().move(South)")
  }

  it should "move North within Range" in {
    val aValidGrid: RobotAt[_1, _0] = Warehouse.withRobotAt[_0, _0]().move(North) //compiles
    val anotherValidGrid: RobotAt[_8, _7] = Warehouse.withRobotAt[_7, _7]().move(North) //compiles

    illTyped("Warehouse.withRobotAt[_9, _5]().move(North)")
  }

  it should "move West within Range" in {
    val aValidGrid: RobotAt[_2, _1] = Warehouse.withRobotAt[_2, _2]().move(West) //compiles

    illTyped("Warehouse.withRobotAt[_3, _0]().move(West)")
  }

  it should "move South several times" in {
    val out: RobotAt[_3, _5] = (South :: South :: HNil).foldLeft(Warehouse.withRobotAt[_5, _5]())(RobotMovesMatcher)

    illTyped("Warehouse.withRobotAt[_1, _5].moveRepeatedly(South :: South :: HNil)")
  }

  it should "move East several times" in {
    val out: RobotAt[_5, _7] = (East :: East :: HNil).foldLeft(Warehouse.withRobotAt[_5, _5]())(RobotMovesMatcher)

    illTyped("Warehouse.withRobotAt[_5, _8].moveRepeatedly(East :: East :: HNil)")
  }

  it should "move North several times" in {
    val out: RobotAt[_7, _5] = (North :: North :: HNil).foldLeft(Warehouse.withRobotAt[_5, _5]())(RobotMovesMatcher)

    illTyped("Warehouse.withRobotAt[_8, _5].moveRepeatedly(North :: North :: HNil)")
  }

  it should "move West several times" in {
    val out: RobotAt[_5, _3] = Warehouse.withRobotAt[_5, _5].moveRepeatedly(West :: West :: HNil)

    illTyped("Warehouse.withRobotAt[_5, _1].moveRepeatedly(West :: West :: HNil)")
  }

  it should "move like Jagger!" in {
    val out: RobotAt[_6, _4] = Warehouse.withRobotAt[_5,_5].moveRepeatedly(West :: West :: North :: North :: South :: East :: HNil)
  }
}

