package org.nisshiee.toban.controller

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current

import scalaz._, Scalaz._

import org.nisshiee.toban.model._

object TaskController extends Controller {

  val taskNameKey = "task_name"
  val taskIdKey = "task_id"

  def index = Action {

    val tasks = DB.withConnection { implicit c => Task.all }
    Ok {
      views.html.Task.index(tasks)
    }
  }

  def detail(id: Int) = Action {

    val taskOpt = DB.withTransaction { implicit c => Task.find(id) }
    val resultOpt: Option[Result] = taskOpt map { t =>
      Ok { views.html.Task.detail(t) }
    }
    resultOpt | Redirect(routes.TaskController.index)
  }

  def create = Action(parse.urlFormEncoded) { implicit req =>

    val taskNameOpt = req.body.get(taskNameKey)
    val taskOpt = taskNameOpt flatMap {
      case Seq(taskName) => DB.withTransaction { implicit c => Task.create(taskName) }
      case _ => None
    }
    val resultOpt: Option[Result] = taskOpt map {
      case Task(id, _) => Redirect(routes.TaskController.detail(id))
    }
    resultOpt | Redirect(routes.TaskController.index)
  }

  def update = Action(parse.urlFormEncoded) { implicit req =>

    val resultOpt: Option[Result] = for {
      id <- req.body.get(taskIdKey) >>= {
        case Seq(s) => s.parseInt.toOption
        case _ => None
      }
      name <- req.body.get(taskNameKey) >>= {
        case Seq(s) => s.some
        case _ => none
      }
      updated <- DB.withTransaction { implicit c =>
        Task.update(id, name)
      }
      result = Redirect(routes.TaskController.detail(id))
    } yield result

    resultOpt | Redirect(routes.TaskController.index)
  }
}
