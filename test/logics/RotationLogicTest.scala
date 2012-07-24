package org.nisshiee.toban.logic

import org.specs2._

import scalaz._, Scalaz._

import org.nisshiee.toban.model._
import org.nisshiee.toban.logic.RotationLogic.{ DeleteError, InsertError, MoveError }
import org.nisshiee.toban.logic.RotationLogic.{ NoId, Full, IllegalPos }

class RotationLogictest extends Specification { def is =

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

  val testEnv = (8, 0)
  val testTask = Task(1, "testtask")
  val testMember1 = Member(1, "member1", Member.Normal)
  val testMember2 = Member(2, "member2", Member.Normal)
  val testMember3 = Member(3, "member3", Member.Normal)
  val testBefore1 = Seq[Rotation]()
  val testBefore2 = Seq(
     Rotation(1, testTask, testMember1, 1, Rotation.On)
    ,Rotation(2, testTask, testMember2, 3, Rotation.On)
    ,Rotation(3, testTask, testMember3, 6, Rotation.On)
  )

  // e1 は DefaultEnvをimplicit paramさせる
  def e1 = RotationLogic.delete(testBefore1)(1) must equalTo(NoId.fail)

  def e2 = ko
  def e3 = ko
  def e4 = ko
  def e5 = ko
  def e6 = ko
  def e7 = ko
  def e8 = ko
  def e9 = ko
  def e10 = ko
  def e11 = ko
  def e12 = ko
}
