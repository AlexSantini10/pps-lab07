package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotCompositionSpec extends AnyFlatSpec with Matchers:

  "RobotWithBattery" should "consume battery for each successful action and stop at zero" in:
    val base = SimpleRobot((0, 0), Direction.North)
    val robot = RobotWithBattery(base, batteryStart = 3, batteryCost = 1)

    robot.battery shouldBe 3
    robot.position shouldBe (0, 0)

    robot.act()
    robot.position shouldBe (0, 1)
    robot.battery shouldBe 2

    robot.act()
    robot.position shouldBe (0, 2)
    robot.battery shouldBe 1

    robot.act()
    robot.position shouldBe (0, 3)
    robot.battery shouldBe 0

    robot.act()
    robot.position shouldBe (0, 3)
    robot.battery shouldBe 0

  it should "not perform actions when battery is insufficient for the configured cost" in:
    val base = SimpleRobot((0, 0), Direction.East)
    val robot = RobotWithBattery(base, batteryStart = 2, batteryCost = 3)

    robot.act()
    robot.position shouldBe (0, 0)
    robot.battery shouldBe 2

  "RobotCanFail" should "always perform the action when failure probability is 0.0" in:
    val base = SimpleRobot((0, 0), Direction.North)
    val robot = RobotCanFail(base, failureProbability = 0.0)

    robot.act()
    robot.position shouldBe (0, 1)

  it should "never perform the action when failure probability is 1.0" in:
    val base = SimpleRobot((0, 0), Direction.North)
    val robot = RobotCanFail(base, failureProbability = 1.0)

    robot.act()
    robot.position shouldBe (0, 0)

  "RobotRepeated" should "perform each action the specified number of times" in:
    val base = SimpleRobot((0, 0), Direction.East)
    val robot = RobotRepeated(base, repetitions = 3)

    robot.act()
    robot.position shouldBe (3, 0)

  "Robot composition" should "combine behaviours correctly (repeat + battery)" in:
    val base = SimpleRobot((0, 0), Direction.North)
    val repeated = RobotRepeated(base, repetitions = 2)
    val robot = RobotWithBattery(repeated, batteryStart = 2, batteryCost = 1)

    robot.act()
    robot.position shouldBe (0, 2)
    robot.battery shouldBe 1

    robot.act()
    robot.position shouldBe (0, 4)
    robot.battery shouldBe 0

    robot.act()
    robot.position shouldBe (0, 4)
    robot.battery shouldBe 0
