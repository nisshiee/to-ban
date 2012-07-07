package org.nisshiee.toban.controller

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current

import scalaz._, Scalaz._, Validation.Monad._

import org.nisshiee.toban.model._
import Member.{ Status, Normal, Deleted, Undefined }
import Member.{ DeleteError, NoMember, InvalidStatus }

object MemberController extends Controller {

  val memberNameKey = "member_name"
  val memberIdKey = "member_id"

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
      case Member(id, _, _) => Redirect(routes.MemberController.detail(id))
    }
    resultOpt | Redirect(routes.MemberController.index)
  }

  def delete = Action(parse.urlFormEncoded) { implicit req =>

    val resultVld: Validation[DeleteError, Result] =
      DB.withTransaction { implicit c =>
        for {
          memberId <- req.body.get(memberIdKey).toSuccess[DeleteError](NoMember) >>= {
            case Seq(s) => s.parseInt.fail.map(_ => NoMember).validation
            case _ => NoMember.fail
          }
          deleted <- Member.delete(memberId)
          result <- Redirect(routes.MemberController.detail(memberId)).success
        } yield result
      }

    resultVld ||| {
      case NoMember => Redirect(routes.MemberController.index)
      case InvalidStatus(Status(id)) => Redirect(routes.MemberController.detail(id))
    }
  }

  def update = Action(parse.urlFormEncoded) { implicit req =>

    val resultOpt: Option[Result] = for {
      memberId <- req.body.get(memberIdKey) >>= {
        case Seq(s) => s.parseInt.toOption
        case _ => none
      }
      newName <- req.body.get(memberNameKey) >>= {
        case Seq(s) => s.some
        case _ => none
      }
      _ <- DB.withTransaction { implicit c =>
        Member.update(memberId, newName)
      }
      result = Redirect(routes.MemberController.detail(memberId))
    } yield result

    resultOpt | Redirect(routes.MemberController.index)
  }
}
