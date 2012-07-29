package org.nisshiee.toban.model

import org.specs2._
import play.api.test._, Helpers._
import play.api.Play.current

import scalaz._, Scalaz._, Validation.Monad._
import play.api.db._

import org.nisshiee.toban.test.TestHelper

class TobanTest extends Specification with TestHelper { def is =

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
                                                                                p^
      "deleteのテスト"                                                          ^
        "存在しないTaskを指定した場合はfalseが返る"                             ! e8^
        "Task, Dateが一致するレコードがなければfalseが返る"                     ! e9^
        "Task, Dateが一致するレコードがあればDeleteされ、trueが返る"            ! e10^
                                                                                p^
      "delete→findのテスト"                                                    ^
        "delete後、そのレコードをfindするとNoneが返る"                          ! e11^
                                                                                p^
                                                                                p^
    "TobanEqualのテスト"                                                        ^
      "taskとdateが一致していればtrue(memberも一致)"                            ! e12^
      "taskとdateが一致していればtrue(memberは異なる)"                          ! e13^
      "taskが異なる場合はfalse"                                                 ! e14^
      "dateが異なる場合はfalse"                                                 ! e15^
                                                                                end

  def e1 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      Toban.find(1, LocalDate.today) must beNone
    }
  }

  def e2 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      val createResult = for {
        m <- Member.create("testmember")
        tobanVld = Toban.replace(1, LocalDate.today, m.id)
      } yield tobanVld
      createResult must beSome.which(Toban.NoTask.fail[Toban] ==)
    }
  }

  def e3 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      val createResult = for {
        t <- Task.create("testtask")
        tobanVld = Toban.replace(t.id, LocalDate.today, 1)
      } yield tobanVld
      createResult must beSome.which(Toban.NoMember.fail[Toban] ==)
    }
  }

  def e4 = runningEmptyApplication {
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

  def e5 = runningEmptyApplication {
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

  def e6 = runningEmptyApplication {
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

  def e7 = runningEmptyApplication {
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

  def e8 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      val today = LocalDate.today
      Toban.delete(1, today) must beFalse
    }
  }

  def e9 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      val today = LocalDate.today
      val resultOpt = for {
        task <- Task.create("testtask")
        result = Toban.delete(task.id, today)
      } yield result
      resultOpt must beSome.which(false ==)
    }
  }

  def e10 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      val today = LocalDate.today
      val resultOpt = for {
        task <- Task.create("testtask")
        member <- Member.create("testmember")
        toban <- Toban.replace(task.id, today, member.id).toOption
        result = Toban.delete(task.id, today)
      } yield result
      resultOpt must beSome.which(true ==)
    }
  }

  def e11 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      val today = LocalDate.today
      val resultOpt = for {
        task <- Task.create("testtask")
        member <- Member.create("testmember")
        toban <- Toban.replace(task.id, today, member.id).toOption
        _ = Toban.delete(task.id, today)
        result = Toban.find(task.id, today)
      } yield result
      resultOpt must beSome.like {
        case r => r must beNone
      }
    }
  }

  def e12 = {
    val t1 = Toban(
       Task(1, "testtask")
      ,new LocalDate("2012-01-01")
      ,Member(2, "testmember", Member.Normal)
    )
    val t2 = Toban(
       Task(1, "testtask")
      ,new LocalDate("2012-01-01")
      ,Member(2, "testmember", Member.Normal)
    )

    (t1 ≟ t2) must beTrue
  }

  def e13 = {
    val t1 = Toban(
       Task(1, "testtask")
      ,new LocalDate("2012-01-01")
      ,Member(2, "testmember", Member.Normal)
    )
    val t2 = Toban(
       Task(1, "testtask")
      ,new LocalDate("2012-01-01")
      ,Member(3, "testmember2", Member.Normal)
    )

    (t1 ≟ t2) must beTrue
  }

  def e14 = {
    val t1 = Toban(
       Task(1, "testtask")
      ,new LocalDate("2012-01-01")
      ,Member(2, "testmember", Member.Normal)
    )
    val t2 = Toban(
       Task(4, "testtask2")
      ,new LocalDate("2012-01-01")
      ,Member(2, "testmember", Member.Normal)
    )

    (t1 ≟ t2) must beFalse
  }

  def e15 = {
    val t1 = Toban(
       Task(1, "testtask")
      ,new LocalDate("2012-01-01")
      ,Member(2, "testmember", Member.Normal)
    )
    val t2 = Toban(
       Task(1, "testtask")
      ,new LocalDate("2012-01-02")
      ,Member(2, "testmember", Member.Normal)
    )

    (t1 ≟ t2) must beFalse
  }
}
