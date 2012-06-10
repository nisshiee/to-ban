package org.nisshiee.toban.model

import scalaz._, Scalaz._
import java.sql.Connection

import org.nisshiee.toban.model.db.MemberDb._

case class Member(id: Int, name: String)

object Member {

  def all(implicit c: Connection) = allSql.list(parser)

  def create(name: String)(implicit c: Connection) =
    createSql.on('name -> name).executeInsert(createKeyParser) |> {
      case id => Member(id, name).some
    }

  def find(id: Int)(implicit c: Connection) =
    findSql.on('id -> id).singleOpt(parser)
}

trait Members {

  implicit lazy val MemberShow = shows[Member] {
    case Member(_, name) => Option(name) | ""
  }

  implicit lazy val MemberEqual = equalBy[Member, Int](_.id)
}