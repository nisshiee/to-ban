package org.nisshiee.toban.controller

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current

import scalaz._, Scalaz._

import org.nisshiee.toban.model._

object MemberController extends Controller {

  val memberNameKey = "member_name"

  def index = Action {

    val members = DB.withConnection { implicit c => Member.all }
    Ok {
      views.html.Member.index(members)
    }
  }

  def detail(id: Int) = Action {

    val memberOpt = DB.withTransaction { implicit c => Member.find(id) }
    val resultOpt: Option[Result] = memberOpt map { m =>
      Ok { views.html.Member.detail(m) }
    }
    resultOpt | Redirect(routes.MemberController.index)
  }

  def create = Action(parse.urlFormEncoded) { implicit req =>

    val memberNameOpt = req.body.get(memberNameKey)
    val memberOpt = memberNameOpt flatMap {
      case Seq(memberName) => DB.withTransaction { implicit c => Member.create(memberName) }
      case _ => None
    }
    val resultOpt: Option[Result] = memberOpt map {
      case Member(id, _) => Redirect(routes.MemberController.detail(id))
    }
    resultOpt | Redirect(routes.MemberController.index)
  }

}
