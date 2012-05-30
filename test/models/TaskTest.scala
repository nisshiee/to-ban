package org.nisshiee.toban.model

import org.specs2._
import play.api.test._, Helpers._
import play.api.Play.current

import scalaz._, Scalaz._
import play.api.db._

class TaskTest extends Specification { def is =

  "Taskケースクラスのテスト"                                                    ^
    "TaskShowのテスト"                                                          ^
      "showsの値はnameと一致(asciiのみ)"                                        ! e1^
      "showsの値はnameと一致(日本語を含む)"                                     ! e2^
      "nameがnullの場合は空文字"                                                ! e3^
                                                                                p^
    "TaskEqualのテスト"                                                         ^
      "idが一致していればtrue(nameも一致)"                                      ! e4^
      "idが一致していればtrue(nameは異なる)"                                    ! e5^
      "idが異なる場合はfalse(nameも異なる)"                                     ! e6^
      "idが異なる場合はfalse(nameは一致)"                                       ! e7^
                                                                                p^
                                                                                p^
  "CRUDテスト"                                                                  ^
    "allのテスト"                                                               ^
      "何もCreateしてなければ空"                                                ! e8^
                                                                                p^
    "createのテスト"                                                            ^
      "createに成功するとtrueが返る"                                            ! e9^
                                                                                p^
    "create→allのテスト"                                                       ^
      "1回createすると、allでそのTaskが返る"                                    ! e10^
      "2回createすると、allで2Taskが返り、idはそれぞれ異なる"                   ! e11^
      "日本語タスク名もcreateできて、allで取得しても化けない"                   ! e12^
                                                                                end

  def e1 = Task(1, "test-task").shows must_== "test-task"
  def e2 = Task(1, "テストタスク").shows must_== "テストタスク"
  def e3 = Task(1, null).shows must_== ""

  def e4 = Task(1, "test-task") ≟ Task(1, "test-task") must beTrue
  def e5 = Task(1, "test-task") ≟ Task(1, "other-task") must beTrue
  def e6 = Task(1, "test-task") ≟ Task(2, "other-task") must beFalse
  def e7 = Task(1, "test-task") ≟ Task(2, "test-task") must beFalse

  def e8 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      Task.all must be empty
    }
  }

  def e9 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      Task.create("testtask") must beTrue
    }
  }

  def e10 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val name = "testtask"
      Task.create(name)
      val all = Task.all

      val spec1 = all must have size(1)
      val spec2 = all ∘ {
        case Task(_, n) => n ≟ name
      } must_== List(true)

      spec1 and spec2
    }
  }

  def e11 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val (name1, name2) = ("testtask1", "testtask2")
      Task.create(name1)
      Task.create(name2)
      val all = Task.all

      val spec1 = all ∘ (_.name) must contain("testtask1", "testtask2").only
      val spec2 = {
        val ids = all ∘ (_.id)
        ids.size must_== ids.toSet.size
      }

      spec1 and spec2
    }
  }

  def e12 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val name = "日本語タスク"
      Task.create(name)
      val all = Task.all

      val spec1 = all must have size(1)
      val spec2 = all ∘ {
        case Task(_, n) => n ≟ name
      } must_== List(true)

      spec1 and spec2
    }
  }
}
