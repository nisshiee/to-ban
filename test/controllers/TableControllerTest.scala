package org.nisshiee.toban.controller

import org.specs2._
import play.api.test._
import play.api.test.Helpers._
import play.api.Play.current
import play.api.db._

import org.nisshiee.toban.model._

class TableControllerTest extends Specification { def is =

  "index"                                                                       ^
    "タスクが登録されていればOKが返る"                                          ! e1^
    "タスクが登録されていない場合は/taskにリダイレクト"                         ! e2^
                                                                                p^
  "week(dateStr)"                                                               ^
    "タスクが登録されていない場合は/taskにリダイレクト"                         ! e3^
    "タスクは登録されているが、引数が日付として不正な場合は/にリダイレクト"     ! e4^
    "タスクも登録されていて、引数も日付として正しい場合はOK"                    ! e5^
                                                                                end

  def e1 = {
    val result = running(FakeApplication()) {
      DB.withTransaction { implicit c =>
        Task.create("testtask")
      }
      TableController.index(FakeRequest())
    }
    status(result) must equalTo(OK)
  }

  def e2 = {
    val result = running(FakeApplication()) {
      TableController.index(FakeRequest())
    }
    redirectLocation(result) must beSome.which("/task" ==)
  }


  def e3 = {
    val result = running(FakeApplication()) {
      TableController.week("2012-06-16")(FakeRequest())
    }
    redirectLocation(result) must beSome.which("/task" ==)
  }

  def e4 = {
    val result = running(FakeApplication()) {
      DB.withTransaction { implicit c =>
        Task.create("testtask")
      }
      TableController.week("2012-06-32")(FakeRequest())
    }
    redirectLocation(result) must beSome.which("/" ==)
  }

  def e5 = {
    val result = running(FakeApplication()) {
      DB.withTransaction { implicit c =>
        Task.create("testtask")
      }
      TableController.week("2012-06-16")(FakeRequest())
    }
    status(result) must equalTo(OK)
  }
}
