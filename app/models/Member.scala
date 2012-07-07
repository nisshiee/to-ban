package org.nisshiee.toban.model

import scalaz._, Scalaz._, Validation.Monad._
import java.sql.Connection

import org.nisshiee.toban.model.db.MemberDb._

case class Member(id: Int, name: String, status: Member.Status)

object Member {

  sealed trait Status
  case object Normal extends Status
  case object Deleted extends Status
  case object Undefined extends Status

  object Status {

    def apply(i: Int) = i match {
      case 0 => Normal
      case 1 => Deleted
      case _ => Undefined
    }

    def unapply(s: Status) = s match {
      case Normal => 0.some
      case Deleted => 1.some
      case Undefined => none
    }
  }

  sealed trait DeleteError
  case object NoMember extends DeleteError
  case class InvalidStatus(s: Status) extends DeleteError

  def all(implicit c: Connection) = allSql.list(parser)

  def create(name: String)(implicit c: Connection) =
    createSql.on('name -> name).executeInsert(createKeyParser) |> {
      case id => Member(id, name, Normal).some
    }

  def find(id: Int)(implicit c: Connection) =
    findSql.on('id -> id).singleOpt(parser)

  def delete(id: Int)(implicit c: Connection) =
    for {
      member <- Member.find(id).toSuccess(NoMember)
      deleted <- deleteSql.on('id -> id).executeUpdate() match {
        case 1 => Member(id, member.name, Member.Deleted).success
        case _ => InvalidStatus(member.status).fail
      }
    } yield deleted

  def update(id: Int, name: String)(implicit c: Connection) =
    updateSql.on(
       'id -> id
      ,'name -> name
    ).executeUpdate() match {
      case 1 => find(id)
      case 0 => none
      case _ => sys.error("updated record must less than 2")
    }
}

trait Members {

  implicit lazy val MemberShow = shows[Member] {
    case Member(_, name, _) => Option(name) | ""
  }

  implicit lazy val MemberEqual = equalBy[Member, Int](_.id)

  implicit lazy val MemberStatusShow = shows[Member.Status] {
    case Member.Normal => "normal"
    case Member.Deleted => "deleted"
    case Member.Undefined => "-"
  }

  import play.api.libs.json._, Json._

  implicit lazy val MemberStatusWrites = new Writes[Member.Status] {
    def writes(status: Member.Status) = status match {
      case Member.Status(i) => toJson(i)
    }
  }

  implicit lazy val MemberStatusReads = new Reads[Member.Status] {
    def reads(js: JsValue) = js.as[Int] |> Member.Status.apply
  }

  implicit lazy val MemberWrites = new Writes[Member] {
    def writes(member: Member) = toJson(
      Map(
         "id" -> toJson(member.id)
        ,"name" -> toJson(member.name)
        ,"status" -> toJson(member.status)
      )
    )
  }

  implicit lazy val MemberReads = new Reads[Member] {
    def reads(js: JsValue) =
      Member(
         (js \ "id").as[Int]
        ,(js \ "name").as[String]
        ,(js \ "status").as[Member.Status]
      )
  }
}
