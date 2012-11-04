package org.nisshiee.toban.controller

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current

import scalaz._, Scalaz._

import org.nisshiee.toban.model._

object TableController extends Controller {

  def index = Action {

    val dates = LocalDate.today.weeklyList
    tableResult(dates)
  }

  def week(dateStr: String) = Action {

    val dateOpt = str2DateOpt(dateStr)
    val resultOpt: Option[Result] = dateOpt ∘ { _.weeklyList |> tableResult }
    resultOpt | Redirect(routes.TableController.index)
  }

  def tableResult(dates: Seq[LocalDate]) = {

    val table = DB.withTransaction { implicit c => Table(dates) }
    val (prevWeekDate, nextWeekDate) = pagenationByDates(dates)
    Ok { views.html.Table.index(table, prevWeekDate, nextWeekDate) }
  }

  def pagenationByDate(base: LocalDate): (LocalDate, LocalDate) =
    (base - 7.day, base + 7.day)

  def pagenationByDates(dates: Seq[LocalDate]): (LocalDate, LocalDate) =
    dates.headOption | LocalDate.today |> pagenationByDate
}
