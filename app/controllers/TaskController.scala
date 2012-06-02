package org.nisshiee.toban.controller

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current

import scalaz._, Scalaz._

import org.nisshiee.toban.model._

object TaskController extends Controller {

  val taskNameKey = "task_name"

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

}
