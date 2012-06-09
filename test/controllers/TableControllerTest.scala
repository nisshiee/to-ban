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
}
