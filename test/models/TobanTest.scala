package org.nisshiee.toban.model

import org.specs2._
import play.api.test._, Helpers._
import play.api.Play.current

import scalaz._, Scalaz._, Validation.Monad._
import play.api.db._

class TobanTest extends Specification { def is =

  "Tobanケースクラスのテスト"                                                   ^
    "CRUDテスト"                                                                ^
      "findのテスト"                                                            ^
        "createされていない状態でのfindはNoneを返す"                            ! e1^
                                                                                p^
      "replaceのテスト"                                                         ^
        "Taskが存在しない場合は失敗(NoTask.failを返す)"                         ! e2^
        "Memberが存在しない場合は失敗(NoMember.failを返す)"                     ! e3^
        "Task, Dateが一致するレコードがなければcreateされ、Toban.successが返る" ! e4^
        "Task, Dateが一致するレコードがあればUpdateされ、Toban.successが返る"   ! e5^
                                                                                p^
      "replace→findのテスト"                                                   ^
        "新規create後、そのレコードをfindできる"                                ! e6^
        "update後、そのレコードをfindすると更新後のTobanを取得"                 ! e7^
                                                                                end

  def e1 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      Toban.find(1, LocalDate.today) must beNone
    }
  }

  def e2 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val createResult = for {
        m <- Member.create("testmember")
        tobanVld = Toban.replace(1, LocalDate.today, m.id)
      } yield tobanVld
      createResult must beSome.which(Toban.NoTask.fail[Toban] ==)
    }
  }

  def e3 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val createResult = for {
        t <- Task.create("testtask")
        tobanVld = Toban.replace(t.id, LocalDate.today, 1)
      } yield tobanVld
      createResult must beSome.which(Toban.NoMember.fail[Toban] ==)
    }
  }

  def e4 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val today = LocalDate.today
      val createResult = for {
        t <- Task.create("testtask")
        m <- Member.create("testmember")
        tobanVld = Toban.replace(t.id, today, m.id)
      } yield (tobanVld, t, m)
      createResult must beSome.which {
        case (Success(toban), t, m) => (toban.task ≟ t) && (toban.date ≟ today) && (toban.member ≟ m)
        case _ => false
      }
    }
  }

  def e5 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val today = LocalDate.today
      val updateResult = for {
        t <- Task.create("testtask")
        m1 <- Member.create("testmember1")
        m2 <- Member.create("testmember2")
        afterVld = Toban.replace(t.id, today, m1.id) >>= { before =>
          Toban.replace(before.task.id, before.date, m2.id)
        }
      } yield (afterVld, t, m2)
      updateResult must beSome.which {
        case (Success(toban), t, m) => (toban.task ≟ t) && (toban.date ≟ today) && (toban.member ≟ m)
        case _ => false
      }
    }
  }

  def e6 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val today = LocalDate.today
      val checkResult = for {
        t <- Task.create("testtask")
        m <- Member.create("testmember")
        createResult = Toban.replace(t.id, today, m.id)
        findResult = Toban.find(t.id, today).toSuccess(Toban.DbError)
      } yield (createResult == findResult)
      checkResult must beSome.which(identity)
    }
  }

  def e7 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val today = LocalDate.today
      val checkResult = for {
        t <- Task.create("testtask")
        m1 <- Member.create("testmember1")
        m2 <- Member.create("testmember2")
        createResult = Toban.replace(t.id, today, m1.id)
        updateResult = Toban.replace(t.id, today, m2.id)
        findResult = Toban.find(t.id, today).toSuccess(Toban.DbError)
      } yield (updateResult == findResult)
      checkResult must beSome.which(identity)
    }
  }
}
