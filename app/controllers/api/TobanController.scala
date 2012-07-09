package org.nisshiee.toban.controller.api

import scalaz._, Scalaz._

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current
import play.api.libs.json._, Json._

import org.nisshiee.toban.model._

object TobanController extends Controller {

  def get(taskId: Int, dateStr: String) = Action {

    val tobanOpt = for {
      date <- str2DateOpt(dateStr)
      toban <- DB.withTransaction { implicit c => Toban.find(taskId, date) }
    } yield toban

    tobanOpt ∘ { t => Ok(toJson(t)) } | Ok(toJson(Map[String, String]()))
  }
}
