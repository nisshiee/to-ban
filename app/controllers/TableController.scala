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

  private[controller] def tableByDates(dates: Seq[LocalDate]): Result = {

    val tasksAssignOpt = getTasksAssign(dates)
    val resultOpt: Option[Result] = tasksAssignOpt ∘ tableResult(dates)
    resultOpt | Redirect(routes.TaskController.index)
  }

  private[controller] def pagenationByDate(base: LocalDate): (LocalDate, LocalDate) =
    (base - 7.day, base + 7.day)

  private[controller] def pagenationByDates(dates: Seq[LocalDate]): (LocalDate, LocalDate) =
    dates.headOption | LocalDate.today |> pagenationByDate

  private[controller] def tableResult(dates: Seq[LocalDate])
  : Tuple2[Seq[Task], Assign] => Result = {
    case (tasks, assign) => {
      val (prevWeekDate, nextWeekDate) = pagenationByDates(dates)
      Ok { views.html.Table.index(dates, tasks, assign, prevWeekDate, nextWeekDate) }
    }
  }

  type Assign = Map[(LocalDate, Task), Member]

  private[controller] def getTasksAssign(dates: Seq[LocalDate]): Option[(Seq[Task], Assign)] =
    DB.withTransaction { implicit c =>
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
}
