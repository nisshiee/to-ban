package org.nisshiee.toban.controller

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current

import scalaz._, Scalaz._, Validation.Monad._

import org.nisshiee.toban.model._
import Member.{ Status, Normal, Deleted, Undefined }
import Member.{ DeleteError, NoMember, InvalidStatus }

object MemberController extends Controller with ControllerHelper {

  def index = Action {

    val members = DB.withConnection { implicit c => Member.all }
    Ok { views.html.Member.index(members) }
  }

  def detail(id: Int) = Action {

    val resultOpt: Option[Result] = for {
      member <- DB.withTransaction { implicit c => Member.find(id) }
      result = Ok { views.html.Member.detail(member) }
    } yield result

    resultOpt | Redirect(routes.MemberController.index)
  }

  def create = Action(parse.urlFormEncoded) { implicit req =>

    val resultOpt: Option[Result] = for {
      memberName <- paramOpt[String](memberNameKey)
      member <- DB.withTransaction { implicit c => Member.create(memberName) }
      result = Redirect(routes.MemberController.detail(member.id))
    } yield result

    resultOpt | Redirect(routes.MemberController.index)
  }

  def delete = Action(parse.urlFormEncoded) { implicit req =>

    val resultVld: Validation[DeleteError, Result] = for {
      memberId <- paramVld[DeleteError, Int](memberIdKey)(NoMember)
      _ <- DB.withTransaction { implicit c => Member.delete(memberId) }
      result = Redirect(routes.MemberController.detail(memberId))
    } yield result

    resultVld ||| {
      case NoMember => Redirect(routes.MemberController.index)
      case InvalidStatus(Status(id)) => Redirect(routes.MemberController.detail(id))
    }
  }

  def update = Action(parse.urlFormEncoded) { implicit req =>

    val resultOpt: Option[Result] = for {
      memberId <- paramOpt[Int](memberIdKey)
      newName <- paramOpt[String](memberNameKey)
      _ <- DB.withTransaction { implicit c => Member.update(memberId, newName) }
      result = Redirect(routes.MemberController.detail(memberId))
    } yield result

    resultOpt | Redirect(routes.MemberController.index)
  }
}
