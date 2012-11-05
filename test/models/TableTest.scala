package org.nisshiee.toban.model

import org.specs2._
import play.api.test._, Helpers._
import play.api.Play.current

import scalaz._, Scalaz._
import play.api.db._

import org.nisshiee.toban.test.TestHelper

class TableTest extends Specification with TestHelper { def is =

  "Tableモデルテスト"                                                           ^
    "row"                                                                       ^
      "引数で指定した日付に関するTableRowを取得"                                ! e1^
                                                                                p^
    "apply"                                                                     ^
      "引数で指定した日付リストを表すTableを取得"                               ! e2^
                                                                                end

  def e1 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      val date = LocalDate.today
      val resultOpt = for {
        task1 <- Task.create("testtask1")
        task2 <- Task.create("testtask2")
        member1 <- Member.create("testmember1")
        member2 <- Member.create("testmember2")
        _ <- Toban.replace(task1.id, date, member1.id).toOption
        _ <- Toban.replace(task2.id, date, member2.id).toOption
        _ = Memo.replace(date, "testmemo")
        TableRow(d, m, a) = Table.row(date, Seq(task1, task2))
      } yield (d == date && m == Some("testmemo") && a == Seq(
         task1 -> Some(member1)
        ,task2 -> Some(member2)
      ))
      resultOpt must beSome.which(identity)
    }
  }

  def e2 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      val date = LocalDate.today
      val resultOpt = for {
        task1 <- Task.create("testtask1")
        task2 <- Task.create("testtask2")
        member1 <- Member.create("testmember1")
        member2 <- Member.create("testmember2")
        _ <- Toban.replace(task1.id, date, member1.id).toOption
        _ <- Toban.replace(task2.id, date, member2.id).toOption
        _ = Memo.replace(date, "testmemo")
        Table(tasks, rows) = Table(Seq(date, date + RichInt(1).day))
      } yield (
        tasks == Seq(task1, task2) &&
        rows == Seq(
           TableRow(date, Some("testmemo"), Seq(task1 -> Some(member1), task2 -> Some(member2)))
          ,TableRow(date + RichInt(1).day, None, Seq(task1 -> None, task2 -> None))
        )
      )
      resultOpt must beSome.which(identity)
    }
  }
}
