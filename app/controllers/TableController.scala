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
    tableByDates(dates)
  }

  def week(dateStr: String) = Action {

    val dateOpt = try { new LocalDate(dateStr).some } catch { case _ => none[LocalDate] }
    val resultOpt: Option[Result] = dateOpt ∘ { _.weeklyList |> tableByDates }
    resultOpt | Redirect(routes.TableController.index)
  }

  private def tableByDates(dates: Seq[LocalDate]): Result = {

    val tasksAssignOpt = DB.withTransaction { implicit c =>
      Task.all match {
        case Nil => None
        case l => (for {
          date <- dates.toList
          task <- l
          toban <- Toban.find(task.id, date)
          member = toban.member
        } yield (date, task) -> member) |> (_.toMap) |> (l -> _) |> (_.some)
      }
    }

    val resultOpt: Option[Result] = tasksAssignOpt ∘ {
      case (tasks, assign) =>
        Ok { views.html.Table.index(dates, tasks, assign) }
    }
    resultOpt | Redirect(routes.TaskController.index)
  }
}
