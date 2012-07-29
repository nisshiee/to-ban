package org.nisshiee.toban.controller

import org.specs2._
import play.api.test._
import play.api.test.Helpers._
import play.api.Play.current
import play.api.db._

import org.nisshiee.toban.model._
import org.nisshiee.toban.test.TestHelper

class TableControllerTest extends Specification with TestHelper { def is =

  "index"                                                                       ^
    "タスクが登録されていればOKが返る"                                          ! e1^
    "タスクが登録されていない場合は/taskにリダイレクト"                         ! e2^
                                                                                p^
  "week(dateStr)"                                                               ^
    "タスクが登録されていない場合は/taskにリダイレクト"                         ! e3^
    "タスクは登録されているが、引数が日付として不正な場合は/にリダイレクト"     ! e4^
    "タスクも登録されていて、引数も日付として正しい場合はOK"                    ! e5^
                                                                                p^
  "getTasksAssign"                                                              ^
    """登録されている全てのタスクに対して、
引数に指定された日付のTobanをDBから取得し、
全タスクリストと、当番をAssignの形式で表現したデータをSomeに包んで返す"""       ! e6^
    "Taskが登録されていない場合はNullを返す"                                    ! e7^
    "引数datesが空の場合、Someが戻るが、Assignは空"                             ! e8^
                                                                                p^
  "pagenationByDate"                                                            ^
    "base日付の7日前、7日後の日付をタプルで返す"                                ! e9^
                                                                                p^
  "pagenationByDates"                                                           ^
    "datesが空の場合は、実行日の7日前、7日後をタプルで返す"                     ! e10^
    "datesが空でない場合は、dates(0)の7日前、7日後をタプルで返す"               ! e11^
                                                                                end

  def e1 = {
    val result = runningEmptyApplication {
      DB.withTransaction { implicit c =>
        Task.create("testtask")
      }
      TableController.index(FakeRequest())
    }
    status(result) must equalTo(OK)
  }

  def e2 = {
    val result = runningEmptyApplication {
      TableController.index(FakeRequest())
    }
    redirectLocation(result) must beSome.which("/task" ==)
  }


  def e3 = {
    val result = runningEmptyApplication {
      TableController.week("2012-06-16")(FakeRequest())
    }
    redirectLocation(result) must beSome.which("/task" ==)
  }

  def e4 = {
    val result = runningEmptyApplication {
      DB.withTransaction { implicit c =>
        Task.create("testtask")
      }
      TableController.week("2012-06-32")(FakeRequest())
    }
    redirectLocation(result) must beSome.which("/" ==)
  }

  def e5 = {
    val result = runningEmptyApplication {
      DB.withTransaction { implicit c =>
        Task.create("testtask")
      }
      TableController.week("2012-06-16")(FakeRequest())
    }
    status(result) must equalTo(OK)
  }

  def e6 = runningEmptyApplication {

    val date = new LocalDate("2012-06-17")

    val check = DB.withConnection { implicit c =>
      for {
        task <- Task.create("testtask")
        member <- Member.create("member")
        toban <- Toban.replace(task.id, date, member.id).toOption
        result <- TableController.getTasksAssign(List(date, date + Period.days(1)))
      } yield {
        val (tasks, assign) = result
        val tasksCheck = tasks must contain(task).only
        val assignCheck = assign.toList must contain(date -> task -> member).only
        tasksCheck and assignCheck
      }
    }

    check must beSome.like { case r => r }
  }

  def e7 = runningEmptyApplication {

    val date = new LocalDate("2012-06-17")

    val check = DB.withConnection { implicit c =>
      for {
        member <- Member.create("member")
        result <- TableController.getTasksAssign(List(date, date + Period.days(1)))
      } yield result
    }

    check must beNone
  }

  def e8 = runningEmptyApplication {

    val date = new LocalDate("2012-06-17")

    val check = DB.withConnection { implicit c =>
      for {
        task <- Task.create("testtask")
        member <- Member.create("member")
        toban <- Toban.replace(task.id, date, member.id).toOption
        result <- TableController.getTasksAssign(List())
      } yield {
        val (tasks, assign) = result
        val tasksCheck = tasks must contain(task).only
        val assignCheck = assign.toList must beEmpty
        tasksCheck and assignCheck
      }
    }

    check must beSome.like { case r => r }
  }

  def e9 = {
    val actual = TableController.pagenationByDate(new LocalDate("2012-06-17"))
    val expected = (new LocalDate("2012-06-10"), new LocalDate("2012-06-24"))
    actual must equalTo(expected)
  }

  def e10 = {
    val actual = TableController.pagenationByDates(Nil)
    val expected = (LocalDate.today - Period.days(7), LocalDate.today + Period.days(7))
    actual must equalTo(expected)
  }

  def e11 = {
    val actual = TableController.pagenationByDates(List(
      new LocalDate("2012-06-17")
      ,new LocalDate("2012-06-18")
    ))
    val expected = (new LocalDate("2012-06-10"), new LocalDate("2012-06-24"))
    actual must equalTo(expected)
  }
}
