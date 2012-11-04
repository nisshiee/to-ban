package org.nisshiee.toban.controller

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current

import scalaz._, Scalaz._
import Validation.Monad._

import org.nisshiee.toban.model._

object DayController extends Controller with ControllerHelper {

  def index = dayDetail(LocalDate.today)

  def day(dateString: String) =
    str2DateOpt(dateString) ∘ dayDetail | Action { Redirect(routes.TableController.index) }

  def dayDetail(date: LocalDate) = Action {
    val memo = DB.withTransaction { implicit c =>
      Memo.find(date)
    } ∘ (_.shows)
    Ok { views.html.Day.detail(date, memo) }
  }

  def updateMemo = Action(parse.urlFormEncoded) { implicit req =>

    val resultOpt: Option[Result] = for {
      date <- paramOpt[LocalDate](dateKey)
      _ = (paramOpt[String](memoKey) | "") match {
        case "" => DB.withTransaction { implicit c => Memo.delete(date) }
        case m => DB.withTransaction { implicit c => Memo.replace(date, m) }
      }
      result = Redirect(routes.TableController.week(date.toString))
    } yield result

    resultOpt | Redirect(routes.TableController.index)
  }
}
