package org.nisshiee.toban.controller

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current

import scalaz._, Scalaz._
import Validation.Monad._

import org.nisshiee.toban.model._

object TobanController extends Controller {

  val taskIdKey = "task_id"
  val dateKey = "date"
  val memberIdKey = "member_id"

  def todayDetail(taskId: Int) = detail(taskId, LocalDate.today.toString)

  def detail(taskId: Int, dateStr: String) = Action {

    val dateOpt = try { new LocalDate(dateStr).some } catch { case _ => none[LocalDate] }
    val argsOpt = dateOpt flatMap { date =>
      DB.withTransaction { implicit c =>
        for {
          task <- Task.find(taskId)
          tobanOpt = Toban.find(taskId, date)
          members = Member.all
        } yield (task, date, tobanOpt, members)
      }
    }
    
    val resultOpt: Option[Result] = argsOpt map {
      case (t, d, to, ms) => Ok { views.html.Toban.detail(t, d, to, ms) }
    }
    resultOpt | Redirect(routes.TaskController.index)
  }

  private def getIntParam[E](key: String)(e: E)(req: Request[Map[String, Seq[String]]]) =
    req.body.get(key).toSuccess(e) >>= {
      case Seq(s) => s.parseInt.fail.map(_ => e).validation
      case _ => e.fail
    }

  private def getTaskId[E] = getIntParam[E](taskIdKey)_

  private def getMemberId[E] = getIntParam[E](memberIdKey)_

  private def getDate[E](e: E)(req: Request[Map[String, Seq[String]]]) =
    req.body.get(dateKey).toSuccess(e) >>= {
      case Seq(s) => try { new LocalDate(s).success } catch { case _ => e.fail }
      case _ => e.fail
    }

  def assign = Action(parse.urlFormEncoded) { req =>

    sealed trait AssignRedirect
    case object TaskList extends AssignRedirect
    case class TobanDetail(taskId: Int, date: LocalDate) extends AssignRedirect

    val tobanVld = for {
      taskId <- getTaskId[AssignRedirect](TaskList)(req)
      date <- getDate[AssignRedirect](TaskList)(req)
      memberId <- getMemberId[AssignRedirect](TobanDetail(taskId, date))(req)
      toban <- DB.withTransaction { implicit c =>
        Toban.replace(taskId, date, memberId).fail ∘ {
          case Toban.NoTask => TaskList
          case Toban.NoMember => TobanDetail(taskId, date)
          case _ => TobanDetail(taskId, date)
        } |> (_.validation)
      }
    } yield toban

    val resultVld = tobanVld map { toban =>
      Redirect(routes.TableController.week(toban.date.toString))
    }
    resultVld ||| {
      case TaskList => Redirect(routes.TaskController.index)
      case TobanDetail(t, d) =>
        Redirect(routes.TobanController.detail(t, d.toString))
    }
  }

  def unassign = Action(parse.urlFormEncoded) { req =>

    sealed trait UnassignRedirect
    case object Root extends UnassignRedirect
    case class Week(date: LocalDate) extends UnassignRedirect

    val redirectVld = for {
      date <- getDate[UnassignRedirect](Root)(req)
      taskId <- getTaskId[UnassignRedirect](Week(date))(req)
      redirect <- DB.withTransaction { implicit c =>
        if (Toban.delete(taskId, date)) Week(date).success else Week(date).fail
      }
    } yield redirect

    redirectVld ||| identity match {
      case Root => Redirect(routes.TableController.index)
      case Week(d) => Redirect(routes.TableController.week(d.toString))
    }
  }
}
