package org.nisshiee.toban.model

import scalaz._, Scalaz._

case class Member(id: Int, name: String)

trait Members {

  implicit lazy val MemberShow = shows[Member] {
    case Member(_, name) => Option(name) | ""
  }

  implicit lazy val MemberEqual = equalBy[Member, Int](_.id)
}
