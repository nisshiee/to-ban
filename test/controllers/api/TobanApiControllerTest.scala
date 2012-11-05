package org.nisshiee.toban.controller.api

import org.specs2._
import play.api.test._
import play.api.test.Helpers._
import play.api.Play.current
import play.api.db._
import play.api.libs.json._, Json._

import play.api.mvc._
import org.nisshiee.toban.model._
import org.nisshiee.toban.test.TestHelper

class TobanApiControllerTest extends Specification with TestHelper { def is =

  "get"                                                                         ^
    "Tobanが登録されていない場合、空ObjectのJSONが返る"                         ! e1^
    "Tobanが登録されている場合、登録されているTobanObjectのJSONで返る"          ! e2^
    "指定された日付が不正な場合、空ObjectのJSONが返る"                          ! e3^
                                                                                end

  type Empty = Map[String, String]
  val empty = Map[String, String]()

  def e1 = {
    val result = runningEmptyApplication {
      TobanController.get(1, "2012-01-01", "")(FakeRequest())
    }
    val resultJs = parse(contentAsString(result))

    (status(result) must equalTo(OK)) and
    (header("Access-Control-Allow-Origin", result) must equalTo(Some("*"))) and
    (resultJs.asOpt[Empty] must beSome.which(empty ==))
  }

  def e2 = runningEmptyApplication {
    val expectedOpt = DB.withConnection { implicit c =>
      for {
        task <- Task.create("testtask")
        date = LocalDate.today
        member <- Member.create("testmember")
        toban <- Toban.replace(task.id, date, member.id).toOption
      } yield toban
    }
    val resultOpt = for {
      toban <- expectedOpt
      result = TobanController.get(toban.task.id, toban.date.toString, "")(FakeRequest())
    } yield result

    resultOpt must beSome.like {
      case result => {
        val resultJs = parse(contentAsString(result))
        (status(result) must equalTo(OK)) and
        (header("Access-Control-Allow-Origin", result) must equalTo(Some("*"))) and
        (resultJs.asOpt[Toban] must equalTo(expectedOpt))
      }
    }
  }

  def e3 = runningEmptyApplication {
    val expectedOpt = DB.withConnection { implicit c =>
      for {
        task <- Task.create("testtask")
        date = LocalDate.today
        member <- Member.create("testmember")
        toban <- Toban.replace(task.id, date, member.id).toOption
      } yield toban
    }
    val resultOpt = for {
      toban <- expectedOpt
      result = TobanController.get(toban.task.id, "2012-02-30", "")(FakeRequest())
    } yield result

    resultOpt must beSome.like {
      case result => {
        val resultJs = parse(contentAsString(result))
        (status(result) must equalTo(OK)) and
        (header("Access-Control-Allow-Origin", result) must equalTo(Some("*"))) and
        (resultJs.asOpt[Empty] must beSome.which(empty ==))
      }
    }
  }
}
