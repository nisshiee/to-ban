package org.nisshiee.toban.controller

import org.specs2._
import play.api.test._
import play.api.test.Helpers._
import play.api.Play.current
import play.api.db._

import org.nisshiee.toban.model._
import org.nisshiee.toban.test.TestHelper

class DayControllerTest extends Specification with TestHelper { def is =

  "day"                                                                                             ^
    "不正な日付をリクエストした場合、/にリダイレクト"                                               ! e1^
    "正常な日付をリクエストした場合、OKを返す"                                                      ! e2^
                                                                                                    p^
  "updateMemo"                                                                                      ^
    "不正な日付をリクエストした場合、/にリダイレクト"                                               ! e3^
    "/week/<更新指定日>にリダイレクト"                                                               ! e4^
                                                                                                    end

  def e1 = {
    val result = runningEmptyApplication {
      DayController.day("2012-14-01")(FakeRequest())
    }
    redirectLocation(result) must beSome.which("/" ==)
  }

  def e2 = {
    val result = runningEmptyApplication {
      DayController.day("2012-10-21")(FakeRequest())
    }
    status(result) must equalTo(OK)
  }

  def e3 = {
    val result = runningEmptyApplication {
      val request = new FakeRequest(
         "POST"
        ,routes.DayController.updateMemo.toString
        ,FakeHeaders()
        ,Map(
           DayController.dateKey -> Seq("2012-14-01")
          ,DayController.memoKey -> Seq("memo")
        )
      )
      DayController.updateMemo(request)
    }
    redirectLocation(result) must beSome.which("/" ==)
  }

  def e4 = {
    val (result, after) = runningEmptyApplication {
      val request = new FakeRequest(
         "POST"
        ,routes.DayController.updateMemo.toString
        ,FakeHeaders()
        ,Map(
           DayController.dateKey -> Seq("2012-10-21")
          ,DayController.memoKey -> Seq("memo")
        )
      )
      (
         DayController.updateMemo(request)
        ,DB.withConnection { implicit c => Memo.find(new LocalDate(2012, 10, 21)) }
      )
    }
    (redirectLocation(result) must beSome.which("/week/2012-10-21" ==)) and
    (after must beSome.like { case Memo(_, m) => m must equalTo("memo") })
  }
}

