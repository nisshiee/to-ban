package org.nisshiee.toban.controller

import org.specs2._
import play.api.test._
import play.api.test.Helpers._
import play.api.Play.current
import play.api.db._
import scalaz._, Scalaz._

import org.nisshiee.toban.model._

class TaskControllerTest extends Specification { def is =

  "index"                                                                       ^
    "OKが返る"                                                                  ! e1^
                                                                                p^
  "detail"                                                                      ^
    "存在しないIDをリクエストした場合、indexにリダイレクト"                     ! e2^
    "存在するタスクをリクエストするとOKが返る"                                  ! e3^
                                                                                p^
  "create"                                                                      ^
    "createされたタスクのdetailにリダイレクト"                                  ! e4^
    "タスク名が指定されていない場合はindexにリダイレクト"                       ! e5^
                                                                                p^
  "update"                                                                      ^
    "存在しないIDをリクエストした場合、indexにリダイレクト"                     ! e6^
    "存在するタスクをリクエストすると、そのタスクのdetailにリダイレクト"        ! e7^
                                                                                end

  def e1 = {
    val result = running(FakeApplication()) {
      TaskController.index(FakeRequest())
    }
    status(result) must equalTo(OK)
  }

  def e2 = {
    val result = running(FakeApplication()) {
      TaskController.detail(1)(FakeRequest())
    }
    redirectLocation(result) must beSome.which("/task" ==)
  }

  def e3 = {
    val result = running(FakeApplication()) {
      val Some(Task(id, _)) = DB.withTransaction { implicit c =>
        Task.create("testtask")
      }
      TaskController.detail(id)(FakeRequest())
    }
    status(result) must equalTo(OK)
  }

  def e4 = {
    val result = running(FakeApplication()) {
      val request = new FakeRequest(
        "POST"
        ,routes.TaskController.create.toString
        ,FakeHeaders()
        ,Map(TaskController.taskNameKey -> Seq("testtask"))
      )
      TaskController.create(request)
    }
    redirectLocation(result) must beSome.which(_ startsWith "/task/detail")
  }

  def e5 = {
    val result = running(FakeApplication()) {
      val request = new FakeRequest(
        "POST"
        ,routes.TaskController.create.toString
        ,FakeHeaders()
        ,Map[String, Seq[String]]()
      )
      TaskController.create(request)
    }
    redirectLocation(result) must beSome.which("/task" ==)
  }

  def e6 = {
    val result = running(FakeApplication()) {
      val request = new FakeRequest(
        "POST"
        ,routes.TaskController.update.toString
        ,FakeHeaders()
        ,Map(
           TaskController.taskIdKey -> Seq("1")
          ,TaskController.taskNameKey -> Seq("newtaskname")
        )
      )
      TaskController.update(request)
    }
    redirectLocation(result) must beSome.which("/task" ==)
  }

  def e7 = {
    val newName = "newtaskname"
    val resultOpt = running(FakeApplication()) {
      val taskIdOpt = DB.withConnection { implicit c =>
        Task.create("testtask") ∘ (_.id)
      }

      taskIdOpt map { taskId =>
        val request = new FakeRequest(
          "POST"
          ,routes.TaskController.update.toString
          ,FakeHeaders()
          ,Map(
             TaskController.taskIdKey -> Seq(taskId.toString)
            ,TaskController.taskNameKey -> Seq(newName)
          )
        )
        TaskController.update(request)
      }
    }

    val checkOpt = for {
      result <- resultOpt
      rlocation <- redirectLocation(result)
    } yield rlocation startsWith "/task/detail"
    checkOpt must beSome.which(identity)
  }
}
