package org.nisshiee.toban.controller

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current

import scalaz._, Scalaz._
import Validation.Monad._

import org.nisshiee.toban.model._

object TobanController extends Controller with ControllerHelper {

  def todayDetail(taskId: Int) = detail(taskId, LocalDate.today.toString)

  def detail(taskId: Int, dateStr: String) = Action {

    val resultOpt: Option[Result] = for {
      date <- str2DateOpt(dateStr)
      (task, date, tobanOpt, members) <- DB.withTransaction { implicit c =>
        for {
          task <- Task.find(taskId)
          tobanOpt = Toban.find(taskId, date)
          members = Member.all
        } yield (task, date, tobanOpt, members)
      }
      result = Ok { views.html.Toban.detail(task, date, tobanOpt, members) }
    } yield result

    resultOpt | Redirect(routes.TaskController.index)
  }

  def assign = Action(parse.urlFormEncoded) { implicit req =>

    sealed trait AssignRedirect
    case object TaskList extends AssignRedirect
    case class TobanDetail(taskId: Int, date: LocalDate) extends AssignRedirect

    val resultVld = for {
      taskId <- paramVld[AssignRedirect, Int](taskIdKey)(TaskList)
      date <- paramVld[AssignRedirect, LocalDate](dateKey)(TaskList)
      memberId <- paramVld[AssignRedirect, Int](memberIdKey)(TobanDetail(taskId, date))
      toban <- DB.withTransaction { implicit c =>
        Toban.replace(taskId, date, memberId).fail ∘ {
          case Toban.NoTask => TaskList
          case Toban.NoMember => TobanDetail(taskId, date)
          case _ => TobanDetail(taskId, date)
        } |> (_.validation)
      }
      result = Redirect(routes.TableController.week(toban.date.toString))
    } yield result

    resultVld ||| {
      case TaskList => Redirect(routes.TaskController.index)
      case TobanDetail(t, d) =>
        Redirect(routes.TobanController.detail(t, d.toString))
    }
  }

  def unassign = Action(parse.urlFormEncoded) { implicit req =>

    sealed trait UnassignRedirect
    case object Root extends UnassignRedirect
    case class Week(date: LocalDate) extends UnassignRedirect

    val resultVld = for {
      date <- paramVld[UnassignRedirect, LocalDate](dateKey)(Root)
      taskId <- paramVld[UnassignRedirect, Int](taskIdKey)(Week(date))
      _ <- DB.withTransaction { implicit c =>
        Toban.delete(taskId, date)
          .option(())
          .toSuccess[UnassignRedirect](Week(date))
      }
      result = Redirect(routes.TableController.week(date.toString))
    } yield result

    resultVld ||| {
      case Root => Redirect(routes.TableController.index)
      case Week(d) => Redirect(routes.TableController.week(d.toString))
    }
  }
}
