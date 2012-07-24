package org.nisshiee.toban.logic

import org.specs2._
import org.specs2.matcher.DataTables

import scalaz._, Scalaz._

import org.nisshiee.toban.model._
import org.nisshiee.toban.logic.RotationLogic.OperationResult
import org.nisshiee.toban.logic.RotationLogic.{ DeleteError, InsertError, MoveError }
import org.nisshiee.toban.logic.RotationLogic.{ NoId, Full, IllegalPos }

class RotationLogictest extends Specification with DataTables { def is =

  "delete"                                                                      ^
    "指定したIDが無い場合はNoId"                                                ! e1^
    "指定したIDがある場合は削除"                                                ! e2^
                                                                                p^
  "insert"                                                                      ^
    "MinScore <-> MaxScore間に空きがない場合はFull"                             ! e3^
    "挿入位置が負の場合はIllegalPos"                                            ! e4^
    "挿入位置が<length>より大きい場合はIllegalPos"                              ! e5^
    "挿入位置のscoreに隙間があれば、中央に挿入"                                 ! e6^
    "挿入位置のscoreに隙間がない場合、挿入後全体を均質化"                       ! e7^
                                                                                p^
  "move"                                                                        ^
    "指定したIDがない場合はNoId"                                                ! e8^
    "挿入位置が負の場合はIllegalPos"                                            ! e9^
    "挿入位置が<length - 1>より大きい場合はIllegalPos"                          ! e10^
    "挿入位置のscoreに隙間があれば、中央に移動"                                 ! e11^
    "挿入位置のscoreに隙間がない場合、移動後全体を均質化"                       ! e12^
                                                                                end

  val testEnv = Rotation.Env(8, 0)
  val testTask = Task(1, "testtask")
  val testMember1 = Member(1, "member1", Member.Normal)
  val testMember2 = Member(2, "member2", Member.Normal)
  val testMember3 = Member(3, "member3", Member.Normal)
  val testMember4 = Member(4, "member4", Member.Normal)
  val testBefore1 = Seq[Rotation]()
  val testBefore2 = Seq(
     Rotation(1, testTask, testMember1, 1, Rotation.On)
    ,Rotation(2, testTask, testMember2, 3, Rotation.On)
    ,Rotation(3, testTask, testMember3, 6, Rotation.On)
  )
  val testBefore3 = Seq(
     Rotation(1, testTask, testMember1, 1, Rotation.On)
    ,Rotation(2, testTask, testMember2, 2, Rotation.On)
    ,Rotation(3, testTask, testMember3, 3, Rotation.On)
  )

  def e1 = RotationLogic.delete(testBefore1)(1) must equalTo(NoId.fail)

  def e2 = RotationLogic.delete(testBefore2)(2) must equalTo {
    OperationResult(
       Seq(
         Rotation(1, testTask, testMember1, 1, Rotation.On)
        ,Rotation(3, testTask, testMember3, 6, Rotation.On)
      )
      ,Seq[Rotation]()
      ,Seq[Rotation]()
      ,Seq(2)
    ).success
  }

  def e3 = {
    val fullBefore = Seq(
       Rotation(1, testTask, testMember1, 1, Rotation.On)
      ,Rotation(2, testTask, testMember2, 3, Rotation.On)
      ,Rotation(3, testTask, testMember3, 6, Rotation.On)
    )
    val fullEnv = Rotation.Env(4, 0)
    RotationLogic.insert(fullBefore)(testTask, testMember4, 2)(fullEnv) must equalTo(Full.fail)
  }

  def e4 =
    "pos" | "result"        |
    -1    ! IllegalPos.fail |
    -2    ! IllegalPos.fail |
    -99   ! IllegalPos.fail |> { (pos, result) =>
      RotationLogic.insert(testBefore2)(testTask, testMember4, pos)(testEnv) must equalTo(result)
    }

  def e5 =
    "pos"                 | "result"        |
    testBefore2.size + 1  ! IllegalPos.fail |
    testBefore2.size + 2  ! IllegalPos.fail |
    testBefore2.size + 99 ! IllegalPos.fail |> { (pos, result) =>
      RotationLogic.insert(testBefore2)(testTask, testMember4, pos)(testEnv) must equalTo(result)
    }

  def e6 = e6_1 and e6_2 and e6_3

  def e6_1 =
    RotationLogic.insert(testBefore2)(testTask, testMember4, 2)(testEnv) must equalTo {
      OperationResult(
         Seq(
            Rotation(1, testTask, testMember1, 1, Rotation.On)
           ,Rotation(2, testTask, testMember2, 3, Rotation.On)// あれ？挿入されるIDは？
           ,Rotation(2, testTask, testMember2, 3, Rotation.On)
           ,Rotation(3, testTask, testMember3, 6, Rotation.On)
         )
        ,Seq[Rotation]()
        ,Seq[Rotation]()
        ,Seq(2)
      ).success
    }
  def e6_2 = ko
  def e6_3 = ko

  def e7 = ko

  def e8 = ko

  def e9 = ko

  def e10 = ko

  def e11 = ko

  def e12 = ko
}
