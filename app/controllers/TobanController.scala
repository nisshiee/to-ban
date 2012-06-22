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

  def assign = Action(parse.urlFormEncoded) { implicit req =>

    val tobanOpt = for {
      taskId <- req.body.get(taskIdKey) >>= {
        case Seq(s) => s.parseInt.toOption
        case _ => None
      }
      date <- req.body.get(dateKey) >>= {
        case Seq(s) => try { new LocalDate(s).some } catch { case _ => none[LocalDate] }
        case _ => None
      }
      memberId <- req.body.get(memberIdKey) >>= {
        case Seq(s) => s.parseInt.toOption
        case _ => None
      }
      toban <- DB.withTransaction { implicit c =>
        Toban.replace(taskId, date, memberId).toOption
      }
    } yield toban

    val resultOpt = tobanOpt map { toban =>
      Redirect(routes.TobanController.detail(toban.task.id, toban.date.toString))
    }
    resultOpt | Redirect(routes.TaskController.index)
  }
}
