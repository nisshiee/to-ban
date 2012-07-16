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

  def update(id: Int, name: String)(implicit c: Connection) =
    updateSql.on(
       'id -> id
      ,'name -> name
    ).executeUpdate match {
      case 1 => find(id)
      case 0 => none
      case _ => sys.error("updated record must less than 2")
    }
}

trait Tasks {

  implicit lazy val TaskShow = shows[Task] {
    case Task(_, name) => Option(name) | ""
  }

  implicit lazy val TaskEqual = equalBy[Task, Int](_.id)

  import play.api.libs.json._, Json._

  implicit lazy val TaskWrites = new Writes[Task] {
    def writes(task: Task) = toJson(
      Map(
         "id" -> toJson(task.id)
        ,"name" -> toJson(task.name)
      )
    )
  }

  implicit lazy val TaskReads = new Reads[Task] {
    def reads(js: JsValue) =
      Task(
         (js \ "id").as[Int]
        ,(js \ "name").as[String]
      )
  }
}
