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
      "createに成功するとSome[Task]が返る"                                      ! e9^
      "2回createに成功すると共にSome[Task]が返り、IDは異なる"                   ! e13^
                                                                                p^
    "create→allのテスト"                                                       ^
      "1回createすると、allでそのTaskが返る"                                    ! e10^
      "2回createすると、allで2Taskが返り、idはそれぞれ異なる"                   ! e11^
      "日本語タスク名もcreateできて、allで取得しても化けない"                   ! e12^
                                                                                p^
    "findのテスト"                                                              ^
      "存在しないIDに対するfindはNoneが返る"                                    ! e14^
                                                                                p^
    "create→findのテスト"                                                      ^
      "1回createし、そのIDでfindすると対象レコードがSome[Task]で返る"           ! e15^
                                                                                p^
    "updateのテスト"                                                            ^
      "存在しないIDに対するupdateはNoneが返る"                                  ! e16^
      "updateに成功するとはSome[Task]が返る"                                    ! e17^
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
      val name = "testtask"
      Task.create(name) must beSome.which(_.name ≟ name)
    }
  }

  def e13 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val name1 = "testtask1"
      val name2 = "testtask2"
      val createResult1 = Task.create(name1)
      val createResult2 = Task.create(name2)

      val validation = for {
        task1 <- createResult1
        task2 <- createResult2
        idCheck = task1.id ≠ task2.id
        nameCheck1 = name1 ≟ task1.name
        nameCheck2 = name2 ≟ task2.name
      } yield (idCheck && nameCheck1 && nameCheck2)
      validation must beSome.which(identity)
    }
  }

  def e10 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val name = "testtask"
      val createResult = Task.create(name)
      val all = Task.all

      val spec1 = all must have size(1)
      val spec2 = all ∘ {
        case t @ Task(_, n) => (n ≟ name) && (t.some ≟ createResult)
      } must_== List(true)

      spec1 and spec2
    }
  }

  def e11 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val (name1, name2) = ("testtask1", "testtask2")
      val createResult1 = Task.create(name1)
      val createResult2 = Task.create(name2)
      val ids = List(createResult1, createResult2).flatten ∘ (_.id)
      val all = Task.all

      val spec1 = all ∘ (_.name) must contain(name1, name2).only
      val spec2 = all ∘ (_.id) must containAllOf(ids).only

      spec1 and spec2
    }
  }

  def e12 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val name = "日本語タスク"
      val createResult = Task.create(name)
      val all = Task.all

      val spec1 = all must have size(1)
      val spec2 = all ∘ {
        case t @ Task(_, n) => (n ≟ name) && (t.some ≟ createResult)
      } must_== List(true)

      spec1 and spec2
    }
  }

  def e14 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      Task.find(1) must beNone
    }
  }

  def e15 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val validation = for {
        t <- Task.create("testtask")
        found <- Task.find(t.id)
      } yield ((t.id ≟ found.id) && (t.name ≟ found.name))
      validation must beSome.which(identity)
    }
  }

  def e16 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      Task.update(1, "testtask")
    } must beNone
  }

  def e17 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      for {
        task <- Task.create("testtask")
        updated <- Task.update(task.id, "updated")
      } yield updated
    } must beSome.like {
      case Task(_, name) => name must equalTo("updated")
    }
  }
}
