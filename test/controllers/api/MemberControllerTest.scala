package org.nisshiee.toban.controller.api

import org.specs2._
import play.api.test._
import play.api.test.Helpers._
import play.api.Play.current
import play.api.db._
import play.api.libs.json._, Json._

import org.nisshiee.toban.model._

class MemberControllerTest extends Specification { def is =

  "getAll"                                                                      ^
    "Memberが登録されていない場合、空ArrayのJSONが返る"                         ! e1^
    "Memberが登録されている場合、登録されている全タスクがID順ArrayのJSONで返る" ! e2^
                                                                                end

  def e1 = {
    val result = running(FakeApplication()) {
      MemberController.getAll("")(FakeRequest())
    }
    val resultJs = parse(contentAsString(result))

    (status(result) must equalTo(OK)) and
    (header("Access-Control-Allow-Origin", result) must equalTo(Some("*"))) and
    (resultJs.asOpt[List[Member]] must beSome.which(_.isEmpty))
  }

  def e2 = running(FakeApplication()) {
    val expectedOpt = DB.withConnection { implicit c =>
      for {
        t1 <- Member.create("testmember1")
        t2 <- Member.create("testmember2")
      } yield List(t1, t2)
    }
    val result = MemberController.getAll("")(FakeRequest())
    val resultJs = parse(contentAsString(result))

    (expectedOpt must beSome.which(_.size == 2)) and
    (status(result) must equalTo(OK)) and
    (header("Access-Control-Allow-Origin", result) must equalTo(Some("*"))) and
    (resultJs.asOpt[List[Member]] must equalTo(expectedOpt))
  }
}
