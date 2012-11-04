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

  private[controller] def tableByDates(dates: Seq[LocalDate]): Result = (for {
    tasksAssign <- getTasksAssign(dates)
    memos = getMemos(dates)
    result: Result = tableResult(dates, memos)(tasksAssign)
  } yield result) | Redirect(routes.TaskController.index)

  private[controller] def pagenationByDate(base: LocalDate): (LocalDate, LocalDate) =
    (base - 7.day, base + 7.day)

  private[controller] def pagenationByDates(dates: Seq[LocalDate]): (LocalDate, LocalDate) =
    dates.headOption | LocalDate.today |> pagenationByDate

  private[controller] def tableResult(dates: Seq[LocalDate], memos: Map[LocalDate, Option[String]])
  : Tuple2[Seq[Task], Assign] => Result = {
    case (tasks, assign) => {
      val (prevWeekDate, nextWeekDate) = pagenationByDates(dates)
      Ok { views.html.Table.index(dates, memos, tasks, assign, prevWeekDate, nextWeekDate) }
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

  private[controller] def getMemos(dates: Seq[LocalDate]): Map[LocalDate, Option[String]] =
    DB.withTransaction { implicit c =>
      dates ∘ { d => d -> Memo.find(d).map(_.memo) } |> (_.toMap)
    }
}
