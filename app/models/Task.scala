package org.nisshiee.toban.model

import scalaz._, Scalaz._
import java.sql.Connection

import org.nisshiee.toban.model.db.TaskDb._

case class Task(id: Int, name: String)

object Task {

  def all(implicit c: Connection) = allSql.list(parser)

  def create(name: String)(implicit c: Connection) =
    createSql.on('name -> name).executeInsert(createKeyParser) |> {
      case id => Task(id, name).some
    }

  def find(id: Int)(implicit c: Connection) = findSql.on('id -> id).singleOpt(parser)
}

trait Tasks {

  implicit lazy val TaskShow = shows[Task] {
    case Task(_, name) => Option(name) | ""
  }

  implicit lazy val TaskEqual = equalBy[Task, Int](_.id)
}
