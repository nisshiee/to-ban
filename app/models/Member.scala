package org.nisshiee.toban.model

import scalaz._, Scalaz._
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

  def all(implicit c: Connection) = allSql.list(parser)

  def create(name: String)(implicit c: Connection) =
    createSql.on('name -> name).executeInsert(createKeyParser) |> {
      case id => Member(id, name, Normal).some
    }

  def find(id: Int)(implicit c: Connection) =
    findSql.on('id -> id).singleOpt(parser)
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
}
