package org.nisshiee.toban.controller

import org.specs2._
import play.api.test._
import play.api.test.Helpers._
import play.api.Play.current
import play.api.db._

import org.nisshiee.toban.model._

class MemberControllerTest extends Specification { def is =

  "index"                                                                       ^
    "OKが返る"                                                                  ! e1^
                                                                                p^
  "detail"                                                                      ^
    "存在しないIDをリクエストした場合、indexにリダイレクト"                     ! e2^
    "存在するタスクをリクエストするとOKが返る"                                  ! e3^
                                                                                p^
  "create"                                                                      ^
    "createされたタスクのdetailにリダイレクト"                                  ! e4^
    "メンバー名が指定されていない場合はindexにリダイレクト"                     ! e5^
                                                                                end

  def e1 = {
    val result = running(FakeApplication()) {
      MemberController.index(FakeRequest())
    }
    status(result) must equalTo(OK)
  }

  def e2 = {
    val result = running(FakeApplication()) {
      MemberController.detail(1)(FakeRequest())
    }
    redirectLocation(result) must beSome.which("/member" ==)
  }

  def e3 = {
    val result = running(FakeApplication()) {
      val Some(Member(id, _, _)) = DB.withTransaction { implicit c =>
        Member.create("testmember")
      }
      MemberController.detail(id)(FakeRequest())
    }
    status(result) must equalTo(OK)
  }

  def e4 = {
    val result = running(FakeApplication()) {
      val request = new FakeRequest(
        "POST"
        ,routes.MemberController.create.toString
        ,FakeHeaders()
        ,Map(MemberController.memberNameKey -> Seq("testmember"))
      )
      MemberController.create(request)
    }
    redirectLocation(result) must beSome.which(_ startsWith "/member/detail")
  }

  def e5 = {
    val result = running(FakeApplication()) {
      val request = new FakeRequest(
        "POST"
        ,routes.MemberController.create.toString
        ,FakeHeaders()
        ,Map[String, Seq[String]]()
      )
      MemberController.create(request)
    }
    redirectLocation(result) must beSome.which("/member" ==)
  }
}
