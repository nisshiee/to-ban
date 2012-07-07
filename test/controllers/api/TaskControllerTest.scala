package org.nisshiee.toban.controller.api

import org.specs2._
import play.api.test._
import play.api.test.Helpers._
import play.api.Play.current
import play.api.db._
import play.api.libs.json._, Json._

import org.nisshiee.toban.model._

class TaskControllerTest extends Specification { def is =

  "getAll"                                                                      ^
    "Taskが登録されていない場合、空ArrayのJSONが返る"                           ! e1^
    "Taskが登録されている場合、登録されている全タスクがID順ArrayのJSONで返る"   ! e2^
                                                                                end

  def e1 = {
    val result = running(FakeApplication()) {
      TaskController.getAll(FakeRequest())
    }
    val resultJs = parse(contentAsString(result))

    (status(result) must equalTo(OK)) and
    (resultJs.asOpt[List[Task]] must beSome.which(_.isEmpty))
  }

  def e2 = running(FakeApplication()) {
    val expectedOpt = DB.withConnection { implicit c =>
      for {
        t1 <- Task.create("testtask1")
        t2 <- Task.create("testtask2")
      } yield List(t1, t2)
    }
    val result = TaskController.getAll(FakeRequest())
    val resultJs = parse(contentAsString(result))

    (expectedOpt must beSome.which(_.size == 2)) and
    (status(result) must equalTo(OK)) and
    (resultJs.asOpt[List[Task]] must equalTo(expectedOpt))
  }
}
