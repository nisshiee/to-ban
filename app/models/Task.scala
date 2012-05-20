package org.nisshiee.toban.model

import scalaz._, Scalaz._

case class Task(id: Int, name: String)

trait Tasks {

  implicit lazy val TaskShow = shows[Task] {
    case Task(_, name) => Option(name) | ""
  }

  implicit lazy val TaskEqual = equalBy[Task, Int](_.id)
}
