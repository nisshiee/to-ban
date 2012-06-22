package org.nisshiee.toban.controller

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current

import scalaz._, Scalaz._

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



  sealed trait AssignRedirect
  case object TaskList extends AssignRedirect
  case class TobanDetail(taskId: Int, date: LocalDate) extends AssignRedirect

  def assign = Action(parse.urlFormEncoded) { implicit req =>

    import Validation.Monad._

    val tobanVld = for {
      taskId <- req.body.get(taskIdKey).toSuccess[AssignRedirect](TaskList) >>= {
        case Seq(s) => s.parseInt.fail.map(_ => TaskList).validation
        case _ => TaskList.fail
      }
      date <- req.body.get(dateKey).toSuccess[AssignRedirect](TaskList) >>= {
        case Seq(s) => try { new LocalDate(s).success } catch { case _ => TaskList.fail }
        case _ => TaskList.fail
      }
      memberId <- req.body.get(memberIdKey).toSuccess[AssignRedirect](TobanDetail(taskId, date)) >>= {
        case Seq(s) => s.parseInt.fail.map(_ => TobanDetail(taskId, date)).validation
        case _ => TobanDetail(taskId, date).fail
      }
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
}
