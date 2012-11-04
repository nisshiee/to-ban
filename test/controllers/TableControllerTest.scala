package org.nisshiee.toban.controller

import org.specs2._
import play.api.test._
import play.api.test.Helpers._
import play.api.Play.current
import play.api.db._

import org.nisshiee.toban.model._
import org.nisshiee.toban.test.TestHelper

class TableControllerTest extends Specification with TestHelper { def is =

  "index"                                                                       ^
    "OKが返る"                                                                  ! e1^
                                                                                p^
  "week(dateStr)"                                                               ^
    "引数が日付として不正な場合は/にリダイレクト"                               ! e4^
    "引数も日付として正しい場合はOK"                                            ! e5^
                                                                                p^
  "pagenationByDate"                                                            ^
    "base日付の7日前、7日後の日付をタプルで返す"                                ! e9^
                                                                                p^
  "pagenationByDates"                                                           ^
    "datesが空の場合は、実行日の7日前、7日後をタプルで返す"                     ! e10^
    "datesが空でない場合は、dates(0)の7日前、7日後をタプルで返す"               ! e11^
                                                                                end

  def e1 = {
    val result = runningEmptyApplication {
      DB.withTransaction { implicit c =>
        Task.create("testtask")
      }
      TableController.index(FakeRequest())
    }
    status(result) must equalTo(OK)
  }

  def e4 = {
    val result = runningEmptyApplication {
      DB.withTransaction { implicit c =>
        Task.create("testtask")
      }
      TableController.week("2012-06-32")(FakeRequest())
    }
    redirectLocation(result) must beSome.which("/" ==)
  }

  def e5 = {
    val result = runningEmptyApplication {
      DB.withTransaction { implicit c =>
        Task.create("testtask")
      }
      TableController.week("2012-06-16")(FakeRequest())
    }
    status(result) must equalTo(OK)
  }

  def e9 = {
    val actual = TableController.pagenationByDate(new LocalDate("2012-06-17"))
    val expected = (new LocalDate("2012-06-10"), new LocalDate("2012-06-24"))
    actual must equalTo(expected)
  }

  def e10 = {
    val actual = TableController.pagenationByDates(Nil)
    val expected = (LocalDate.today - Period.days(7), LocalDate.today + Period.days(7))
    actual must equalTo(expected)
  }

  def e11 = {
    val actual = TableController.pagenationByDates(List(
      new LocalDate("2012-06-17")
      ,new LocalDate("2012-06-18")
    ))
    val expected = (new LocalDate("2012-06-10"), new LocalDate("2012-06-24"))
    actual must equalTo(expected)
  }
}
