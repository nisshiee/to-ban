package org.nisshiee.toban.controller

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current

import scalaz._, Scalaz._

import org.nisshiee.toban.model._

object TaskController extends Controller with ControllerHelper {

  def index = Action {

    val tasks = DB.withConnection { implicit c => Task.all }
    Ok { views.html.Task.index(tasks) }
  }

  def detail(id: Int) = Action {

    val resultOpt: Option[Result] = for {
      task <- DB.withTransaction { implicit c => Task.find(id) }
      result = Ok { views.html.Task.detail(task) }
    } yield result

    resultOpt | Redirect(routes.TaskController.index)
  }

  def create = Action(parse.urlFormEncoded) { implicit req =>

    val resultOpt: Option[Result] = for {
      taskName <- paramOpt[String](taskNameKey)
      task <- DB.withTransaction { implicit c => Task.create(taskName) }
      result = Redirect(routes.TaskController.detail(task.id))
    } yield result

    resultOpt | Redirect(routes.TaskController.index)
  }

  def update = Action(parse.urlFormEncoded) { implicit req =>

    val resultOpt: Option[Result] = for {
      id <- paramOpt[Int](taskIdKey)
      name <- paramOpt[String](taskNameKey)
      updated <- DB.withTransaction { implicit c => Task.update(id, name) }
      result = Redirect(routes.TaskController.detail(id))
    } yield result

    resultOpt | Redirect(routes.TaskController.index)
  }
}
