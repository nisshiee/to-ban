package org.nisshiee.toban.model

import org.specs2._
import scalaz._, Scalaz._

class TaskTest extends Specification with Tasks { def is =

  "Taskケースクラスのテスト"                                ^
    "TaskShowのテスト"                                      ^
      "showsの値はnameと一致(asciiのみ)"                    ! e1^
      "showsの値はnameと一致(日本語を含む)"                 ! e2^
      "nameがnullの場合は空文字"                            ! e3^
                                                            p^
    "TaskEqualのテスト"                                     ^
      "idが一致していればtrue(nameも一致)"                  ! e4^
      "idが一致していればtrue(nameは異なる)"                ! e5^
      "idが異なる場合はfalse(nameも異なる)"                 ! e6^
      "idが異なる場合はfalse(nameは一致)"                   ! e7^
                                                            end

  def e1 = Task(1, "test-task").shows must_== "test-task"
  def e2 = Task(1, "テストタスク").shows must_== "テストタスク"
  def e3 = Task(1, null).shows must_== ""

  def e4 = Task(1, "test-task") ≟ Task(1, "test-task") must beTrue
  def e5 = Task(1, "test-task") ≟ Task(1, "other-task") must beTrue
  def e6 = Task(1, "test-task") ≟ Task(2, "other-task") must beFalse
  def e7 = Task(1, "test-task") ≟ Task(2, "test-task") must beFalse
}
  