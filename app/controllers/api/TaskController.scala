package org.nisshiee.toban.controller.api

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current
import play.api.libs.json._, Json._
import play.api.libs.Jsonp

import org.nisshiee.toban.model._
import org.nisshiee.toban.controller.ControllerHelper

object TaskController extends Controller with ControllerHelper {

  def getAll(callback: String) = ApiAction {
    val tasks = DB.withTransaction { implicit c =>
      Task.all
    }
    callback match {
      case "" => Ok(toJson(tasks))
      case c => Ok(Jsonp(c, toJson(tasks)))
    }
  }
}
