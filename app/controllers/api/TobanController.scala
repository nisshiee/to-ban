package org.nisshiee.toban.controller.api

import scalaz._, Scalaz._

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current
import play.api.libs.json._, Json._
import play.api.libs.Jsonp

import org.nisshiee.toban.model._

object TobanController extends Controller {

  def get(taskId: Int, dateStr: String, callback: String) = Action {

    val tobanOpt = for {
      date <- str2DateOpt(dateStr)
      toban <- DB.withTransaction { implicit c => Toban.find(taskId, date) }
    } yield toban

    tobanOpt ∘ toJson[Toban] | toJson(Map[String, String]()) |> { js =>
      callback match {
        case "" => Ok(js)
        case c => Ok(Jsonp(c, js))
      }
    }
  }
}
