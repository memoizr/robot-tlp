package robot

import shapeless._
import org.scalatest.{FlatSpec, Matchers}
import shapeless.Nat._
import shapeless.test.illTyped

import scala.language.higherKinds

class RobotTest extends FlatSpec with Matchers {

  it should "at most be ten by ten" in {
    val gridMin: RobotAt[_0, _0] = Warehouse.withRobotAt[_0, _0]()
    val gridMax: RobotAt[_9, _9] = Warehouse.withRobotAt[_9, _9]()

    illTyped("Warehouse.withRobotAt[_10, _0]", "Illegal position: .*_10 is not <= _9")
    illTyped("Warehouse.withRobotAt[_0, _10]", "Illegal position: .*_10 is not <= _9")
    illTyped("Warehouse.withRobotAt[_10, _10]", "Illegal position: .*_10 is not <= _9")
  }

  it should "move East within Range" in {
    val aValidGrid: RobotAt[_0, _1] = Warehouse.withRobotAt[_0, _0]().move(East)
    val anotherValidGrid: RobotAt[_8, _9] = Warehouse.withRobotAt[_8, _8]().move(East)

    illTyped(
      "Warehouse.withRobotAt[_5, _9]().move(East)",
      "Illegal move: Attempting to move robot at position: latitude=.*._5, longitude=.*_9 towards direction=.*East.*"
    )
  }

  it should "move South within Range" in {
    val aValidGrid: RobotAt[_8, _0] = Warehouse.withRobotAt[_9, _0]().move(South)
    val anotherValidGrid: RobotAt[_1, _1] = Warehouse.withRobotAt[_2, _1]().move(South)

    illTyped(
      "Warehouse.withRobotAt[_0, _5]().move(South)",
      "Illegal move: Attempting to move robot at position: latitude=.*_0, longitude=.*_5 towards direction=.*South.*"
    )
  }

  it should "move North within Range" in {
    val aValidGrid: RobotAt[_1, _0] = Warehouse.withRobotAt[_0, _0]().move(North)
    val anotherValidGrid: RobotAt[_8, _7] = Warehouse.withRobotAt[_7, _7]().move(North)

    illTyped(
      "Warehouse.withRobotAt[_9, _5]().move(North)",
      "Illegal move: Attempting to move robot at position: latitude=.*_9, longitude=.*_5 towards direction=.*North.*"
    )
  }

  it should "move West within Range" in {
    val aValidGrid: RobotAt[_2, _1] = Warehouse.withRobotAt[_2, _2]().move(West)

    illTyped(
      "Warehouse.withRobotAt[_3, _0]().move(West)",
      "Illegal move: Attempting to move robot at position: latitude=.*_3, longitude=.*_0 towards direction=.*West.*"
    )
  }

  it should "move South several times" in {
    val out: RobotAt[_3, _5] = (South :: South :: HNil).foldLeft(Warehouse.withRobotAt[_5, _5]())(Router)

    illTyped(
      "Warehouse.withRobotAt[_1, _5].moveAlongRoute(South :: South :: HNil)",
      """Illegal route: cannot move along route=(.*South.*South).* from position=(.*_1,.*_5).* when using Router=.*\.
Please ensure robot stays within the confines of the warehouse \(_0 <= position <= _9\)."""
    )
  }

  it should "move East several times" in {
    val out: RobotAt[_5, _7] = (East :: East :: HNil).foldLeft(Warehouse.withRobotAt[_5, _5]())(Router)

    illTyped(
      "Warehouse.withRobotAt[_5, _8].moveAlongRoute(East :: East :: HNil)",
      """Illegal route: cannot move along route=(.*East.*East).* from position=(.*_5,.*_8).* when using Router=.*\.
Please ensure robot stays within the confines of the warehouse \(_0 <= position <= _9\)."""
    )
  }

  it should "move North several times" in {
    val out: RobotAt[_7, _5] = (North :: North :: HNil).foldLeft(Warehouse.withRobotAt[_5, _5]())(Router)

    illTyped(
      "Warehouse.withRobotAt[_8, _5].moveAlongRoute(North :: North :: HNil)",
      """Illegal route: cannot move along route=(.*North.*North).* from position=(.*_8,.*_5).* when using Router=.*\.
Please ensure robot stays within the confines of the warehouse \(_0 <= position <= _9\)."""
    )
  }

  it should "move West several times" in {
    val out: RobotAt[_5, _3] = Warehouse.withRobotAt[_5, _5].moveAlongRoute(West :: West :: HNil)

    illTyped(
      "Warehouse.withRobotAt[_5, _1].moveAlongRoute(West :: West :: HNil)",
      """Illegal route: cannot move along route=(.*West.*West).* from position=(.*_5,.*_1).* when using Router=.*\.
Please ensure robot stays within the confines of the warehouse \(_0 <= position <= _9\)."""
    )
  }

  it should "move like Jagger!" in {
    val out: RobotAt[_6, _4] = Warehouse.withRobotAt[_5, _5].moveAlongRoute(West :: West :: North :: North :: South :: East :: HNil)
  }
}

