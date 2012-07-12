package org.nisshiee.toban.controller.api

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current
import play.api.libs.json._, Json._
import play.api.libs.Jsonp

import org.nisshiee.toban.model._

object MemberController extends Controller {

  def getAll(callback: String) = Action {
    val members = DB.withTransaction { implicit c =>
      Member.all
    }
    callback match {
      case "" => Ok(toJson(members))
      case c => Ok(Jsonp(c, toJson(members)))
    }
  }
}
