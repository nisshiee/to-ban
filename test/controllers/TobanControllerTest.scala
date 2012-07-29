package org.nisshiee.toban.controller

import org.specs2._
import play.api.test._
import play.api.test.Helpers._
import play.api.Play.current
import play.api.db._

import org.nisshiee.toban.model._
import org.nisshiee.toban.test.TestHelper

class TobanControllerTest extends Specification with TestHelper { def is =

  "todayDetail"                                                                                     ^
    "存在しないタスクIDをリクエストした場合、/taskにリダイレクト"                                   ! e1^
    "存在するタスクIDをリクエストした場合、OKを返す"                                                ! e2^
                                                                                                    p^
  "detail"                                                                                          ^
    "存在しないタスクIDをリクエストした場合、/taskにリダイレクト"                                   ! e3^
    "存在するタスクIDをリクエストした場合、OKを返す"                                                ! e4^
    "不正な日付をリクエストした場合、/taskにリダイレクト"                                           ! e5^
                                                                                                    p^
  "assign"                                                                                          ^
    "存在しないタスクIDをリクエストした場合、/taskにリダイレクト"                                   ! e6^
    "存在しないメンバーIDをリクエストした場合、元の/toban/detailにリダイレクト"                     ! e7^
    "不正な日付をリクエストした場合、/taskにリダイレクト"                                           ! e8^
    "成功したら/week/<対象日>にリダイレクト"                                                        ! e9^
    "メンバーが選択されていない場合、元の/toban/detailにリダイレクト"                               ! e10^
                                                                                                    p^
  "unassign"                                                                                        ^
    "不正な日付をリクエストした場合、/にリダイレクト"                                               ! e11^
    "存在しないタスクIDをリクエストした場合、/week/<対象日>にリダイレクト"                          ! e12^
    "未アサインなタスク、日付をリクエストした場合、/week/<対象日>にリダイレクト"                    ! e13^
    "成功したら/week/<対象日>にリダイレクト"                                                        ! e14^
                                                                                                    end

  def e1 = {
    val result = runningEmptyApplication {
      TobanController.todayDetail(1)(FakeRequest())
    }
    redirectLocation(result) must beSome.which("/task" ==)
  }

  def e2 = {
    val result = runningEmptyApplication {
      val Some(Task(id, _)) = DB.withTransaction { implicit c =>
        Task.create("testtask")
      }
      TobanController.todayDetail(id)(FakeRequest())
    }
    status(result) must equalTo(OK)
  }

  def e3 = {
    val result = runningEmptyApplication {
      TobanController.detail(1, "2012-01-01")(FakeRequest())
    }
    redirectLocation(result) must beSome.which("/task" ==)
  }

  def e4 = {
    val result = runningEmptyApplication {
      val Some(Task(id, _)) = DB.withTransaction { implicit c =>
        Task.create("testtask")
      }
      TobanController.detail(id, "2012-01-01")(FakeRequest())
    }
    status(result) must equalTo(OK)
  }

  def e5 = {
    val result = runningEmptyApplication {
      val Some(Task(id, _)) = DB.withTransaction { implicit c =>
        Task.create("testtask")
      }
      TobanController.detail(id, "2012-14-01")(FakeRequest())
    }
    redirectLocation(result) must beSome.which("/task" ==)
  }

  def e6 = {
    val result = runningEmptyApplication {
      val (taskId, memberId) = DB.withTransaction { implicit c =>
        val Some(Member(memberId, _, _)) = Member.create("testmember")
        (1, memberId)
      }
      val dateStr = "2012-01-01"
      val request = new FakeRequest(
        "POST"
        ,routes.TobanController.assign.toString
        ,FakeHeaders()
        ,Map(
          TobanController.taskIdKey -> Seq(taskId.toString)
          ,TobanController.dateKey -> Seq(dateStr)
          ,TobanController.memberIdKey -> Seq(memberId.toString)
        )
      )
      TobanController.assign(request)
    }
    redirectLocation(result) must beSome.which("/task" ==)
  }

  def e7 = {
    val dateStr = "2012-01-01"
    val (result, taskId) = runningEmptyApplication {
      val (taskId, memberId) = DB.withTransaction { implicit c =>
        val Some(Task(taskId, _)) = Task.create("testtask")
        (taskId, 1)
      }
      val request = new FakeRequest(
        "POST"
        ,routes.TobanController.assign.toString
        ,FakeHeaders()
        ,Map(
          TobanController.taskIdKey -> Seq(taskId.toString)
          ,TobanController.dateKey -> Seq(dateStr)
          ,TobanController.memberIdKey -> Seq(memberId.toString)
        )
      )
      TobanController.assign(request) -> taskId
    }

    val expected = "/toban/" + taskId + "/" + dateStr
    redirectLocation(result) must beSome.which(expected ==)
  }

  def e8 = {
    val result = runningEmptyApplication {
      val (taskId, memberId) = DB.withTransaction { implicit c =>
        val Some(Task(taskId, _)) = Task.create("testtask")
        val Some(Member(memberId, _, _)) = Member.create("testmember")
        (taskId, memberId)
      }
      val dateStr = "2012-01-50"
      val request = new FakeRequest(
        "POST"
        ,routes.TobanController.assign.toString
        ,FakeHeaders()
        ,Map(
          TobanController.taskIdKey -> Seq(taskId.toString)
          ,TobanController.dateKey -> Seq(dateStr)
          ,TobanController.memberIdKey -> Seq(memberId.toString)
        )
      )
      TobanController.assign(request)
    }
    redirectLocation(result) must beSome.which("/task" ==)
  }

  def e9 = {
    val (result, taskId, dateStr) = runningEmptyApplication {
      val (taskId, memberId) = DB.withTransaction { implicit c =>
        val Some(Task(taskId, _)) = Task.create("testtask")
        val Some(Member(memberId, _, _)) = Member.create("testmember")
        (taskId, memberId)
      }
      val dateStr = "2012-01-01"
      val request = new FakeRequest(
        "POST"
        ,routes.TobanController.assign.toString
        ,FakeHeaders()
        ,Map(
          TobanController.taskIdKey -> Seq(taskId.toString)
          ,TobanController.dateKey -> Seq(dateStr)
          ,TobanController.memberIdKey -> Seq(memberId.toString)
        )
      )
      (TobanController.assign(request), taskId, dateStr)
    }
    redirectLocation(result) must beSome.which("/week/%s".format(dateStr) ==)
  }

  def e10 = {
    val dateStr = "2012-01-01"
    val (result, taskId) = runningEmptyApplication {
      val (taskId, memberId) = DB.withTransaction { implicit c =>
        val Some(Task(taskId, _)) = Task.create("testtask")
        (taskId, 1)
      }
      val request = new FakeRequest(
        "POST"
        ,routes.TobanController.assign.toString
        ,FakeHeaders()
        ,Map(
          TobanController.taskIdKey -> Seq(taskId.toString)
          ,TobanController.dateKey -> Seq(dateStr)
        )
      )
      TobanController.assign(request) -> taskId
    }

    val expected = "/toban/" + taskId + "/" + dateStr
    redirectLocation(result) must beSome.which(expected ==)
  }

  def e11 = {
    val dateStr = "2012-01-50"
    val result = runningEmptyApplication {
      val request = new FakeRequest(
        "POST"
        ,routes.TobanController.unassign.toString
        ,FakeHeaders()
        ,Map(
          TobanController.taskIdKey -> Seq("1")
          ,TobanController.dateKey -> Seq(dateStr)
        )
      )
      TobanController.unassign(request)
    }
    redirectLocation(result) must beSome.which("/" ==)
  }

  def e12 = {
    val dateStr = "2012-01-01"
    val result = runningEmptyApplication {
      val request = new FakeRequest(
        "POST"
        ,routes.TobanController.unassign.toString
        ,FakeHeaders()
        ,Map(
          TobanController.taskIdKey -> Seq("1")
          ,TobanController.dateKey -> Seq(dateStr)
        )
      )
      TobanController.unassign(request)
    }
    val expected = "/week/" + dateStr
    redirectLocation(result) must beSome.which(expected ==)
  }

  def e13 = {
    val dateStr = "2012-01-01"
    val resultOpt = runningEmptyApplication {
      for {
        task <- DB.withTransaction { implicit c => Task.create("testtask") }
        request = new FakeRequest(
          "POST"
          ,routes.TobanController.unassign.toString
          ,FakeHeaders()
          ,Map(
            TobanController.taskIdKey -> Seq(task.id.toString)
            ,TobanController.dateKey -> Seq(dateStr)
          )
        )
      } yield TobanController.unassign(request)
    }
    resultOpt must beSome.like {
      case result => {
        val expected = "/week/" + dateStr
        redirectLocation(result) must beSome.which(expected ==)
      }
    }
  }

  def e14 = {
    val dateStr = "2012-01-01"
    val date = new LocalDate(dateStr)
    val resultOpt = runningEmptyApplication {
      for {
        task <- DB.withTransaction { implicit c => Task.create("testtask") }
        member <- DB.withTransaction { implicit c => Member.create("testmember") }
        _ <- DB.withTransaction { implicit c => Toban.replace(task.id, date, member.id).toOption }
        request = new FakeRequest(
          "POST"
          ,routes.TobanController.unassign.toString
          ,FakeHeaders()
          ,Map(
            TobanController.taskIdKey -> Seq(task.id.toString)
            ,TobanController.dateKey -> Seq(dateStr)
          )
        )
      } yield TobanController.unassign(request)
    }
    resultOpt must beSome.like {
      case result => {
        val expected = "/week/" + dateStr
        redirectLocation(result) must beSome.which(expected ==)
      }
    }
  }
}
