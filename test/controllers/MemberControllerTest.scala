package org.nisshiee.toban.controller

import org.specs2._
import play.api.test._
import play.api.test.Helpers._
import play.api.Play.current
import play.api.db._
import scalaz._, Scalaz._

import org.nisshiee.toban.model._
import org.nisshiee.toban.test.TestHelper

class MemberControllerTest extends Specification with TestHelper { def is =

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
                                                                                p^
  "delete"                                                                      ^
    "存在しないIDをリクエストした場合、indexにリダイレクト"                     ! e6^
    "存在するタスクをリクエストすると、そのタスクのdetailにリダイレクト"        ! e7^
                                                                                p^
  "update"                                                                      ^
    "存在しないIDをリクエストした場合、indexにリダイレクト"                     ! e8^
    "存在するタスクをリクエストすると、そのタスクのdetailにリダイレクト"        ! e9^
                                                                                p^
  "changeColor"                                                                 ^
    "存在しないIDをリクエストした場合、indexにリダイレクト"                     ! e10^
    "Undefinedなcolorをリクエストすると、そのmemberのdetailにリダイレクト"      ! e11^
    "正常なリクエストすると、そのmemberのdetailにリダイレクト"                  ! e12^
                                                                                end

  def e1 = {
    val result = runningEmptyApplication {
      MemberController.index(FakeRequest())
    }
    status(result) must equalTo(OK)
  }

  def e2 = {
    val result = runningEmptyApplication {
      MemberController.detail(1)(FakeRequest())
    }
    redirectLocation(result) must beSome.which("/member" ==)
  }

  def e3 = {
    val result = runningEmptyApplication {
      val Some(Member(id, _, _, _)) = DB.withTransaction { implicit c =>
        Member.create("testmember")
      }
      MemberController.detail(id)(FakeRequest())
    }
    status(result) must equalTo(OK)
  }

  def e4 = {
    val result = runningEmptyApplication {
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
    val result = runningEmptyApplication {
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

  def e6 = {
    val result = runningEmptyApplication {
      val request = new FakeRequest(
        "POST"
        ,routes.MemberController.delete.toString
        ,FakeHeaders()
        ,Map(MemberController.memberIdKey -> Seq("1"))
      )
      MemberController.delete(request)
    }
    redirectLocation(result) must beSome.which("/member" ==)
  }

  def e7 = {
    val resultOpt = runningEmptyApplication {
      val memberIdOpt = DB.withConnection { implicit c =>
        Member.create("testmember") ∘ (_.id)
      }

      memberIdOpt map { memberId =>
        val request = new FakeRequest(
          "POST"
          ,routes.MemberController.delete.toString
          ,FakeHeaders()
          ,Map(MemberController.memberIdKey -> Seq(memberId.toString))
        )
        MemberController.delete(request)
      }
    }

    val checkOpt = for {
      result <- resultOpt
      rlocation <- redirectLocation(result)
    } yield rlocation startsWith "/member/detail"
    checkOpt must beSome.which(identity)
  }

  def e8 = {
    val result = runningEmptyApplication {
      val request = new FakeRequest(
        "POST"
        ,routes.MemberController.update.toString
        ,FakeHeaders()
        ,Map(
           MemberController.memberIdKey -> Seq("1")
          ,MemberController.memberNameKey -> Seq("newmembername")
        )
      )
      MemberController.update(request)
    }
    redirectLocation(result) must beSome.which("/member" ==)
  }

  def e9 = {
    val newName = "newmembername"
    val resultOpt = runningEmptyApplication {
      val memberIdOpt = DB.withConnection { implicit c =>
        Member.create("testmember") ∘ (_.id)
      }

      memberIdOpt map { memberId =>
        val request = new FakeRequest(
          "POST"
          ,routes.MemberController.update.toString
          ,FakeHeaders()
          ,Map(
             MemberController.memberIdKey -> Seq(memberId.toString)
            ,MemberController.memberNameKey -> Seq(newName)
          )
        )
        MemberController.update(request)
      }
    }

    val checkOpt = for {
      result <- resultOpt
      rlocation <- redirectLocation(result)
    } yield rlocation startsWith "/member/detail"
    checkOpt must beSome.which(identity)
  }

  def e10 = {
    val result = runningEmptyApplication {
      val request = new FakeRequest(
        "POST"
        ,routes.MemberController.changeColor.toString
        ,FakeHeaders()
        ,Map(
           MemberController.memberIdKey -> Seq("1")
          ,MemberController.memberColorKey -> Seq("0")
        )
      )
      MemberController.changeColor(request)
    }
    redirectLocation(result) must beSome.which("/member" ==)
  }

  def e11 = {
    val resultOpt = runningEmptyApplication {
      val memberIdOpt = DB.withConnection { implicit c =>
        Member.create("testmember") ∘ (_.id)
      }

      memberIdOpt map { memberId =>
        val request = new FakeRequest(
           "POST"
          ,routes.MemberController.changeColor.toString
          ,FakeHeaders()
          ,Map(
             MemberController.memberIdKey -> Seq(memberId.toString)
            ,MemberController.memberColorKey -> Seq("99")
          )
        )
        redirectLocation(MemberController.changeColor(request)) must beSome.which("/member/detail/" + memberId.toString ==)
      }
    }
    resultOpt must beSome.like { case a => a }
  }

  def e12 = {
    val resultOpt = runningEmptyApplication {
      val memberIdOpt = DB.withConnection { implicit c =>
        Member.create("testmember") ∘ (_.id)
      }

      memberIdOpt map { memberId =>
        val request = new FakeRequest(
           "POST"
          ,routes.MemberController.changeColor.toString
          ,FakeHeaders()
          ,Map(
             MemberController.memberIdKey -> Seq(memberId.toString)
            ,MemberController.memberColorKey -> Seq("1")
          )
        )
        redirectLocation(MemberController.changeColor(request)) must beSome.which("/member/detail/" + memberId.toString ==)
      }
    }
    resultOpt must beSome.like { case a => a }
  }
}
