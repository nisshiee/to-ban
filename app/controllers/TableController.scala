package org.nisshiee.toban.controller

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current

import scalaz._, Scalaz._

import org.nisshiee.toban.model._

object TableController extends Controller {

  def index = Action {

    val dates = (-5 to 5).toList ∘ { i => LocalDate.today + i.day }

    val tasksAssignOpt = DB.withTransaction { implicit c =>
      Task.all match {
        case Nil => None
        case l => (for {
          date <- dates
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
