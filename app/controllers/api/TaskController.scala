package org.nisshiee.toban.controller.api

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current
import play.api.libs.json._, Json._

import org.nisshiee.toban.model._

object TaskController extends Controller {

  def getAll = Action {
    val tasks = DB.withTransaction { implicit c =>
      Task.all
    }
    Ok(toJson(tasks))
  }
}
