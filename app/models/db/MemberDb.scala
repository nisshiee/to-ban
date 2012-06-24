package org.nisshiee.toban.model.db

import anorm._, SqlParser._
import org.nisshiee.toban.model._

object MemberDb {

  val parser: RowParser[Member] = int("id") ~ str("name") ~ int("status") map {
    case id ~ name ~ s => Member(id, name, Member.Status(s))
  }

  val createSql = SQL("""
INSERT INTO member
  SET
    name = {name}
""")

  val createKeyParser = int("null.SCOPE_IDENTITY()").single

  val allSql = SQL("""
SELECT
    id, name, status
  FROM
    member
  WHERE
    status = {status}
""").on {
    val Member.Status(i) = Member.Normal
    'status -> i
  }

  val findSql = SQL("""
SELECT
    id, name, status
  FROM
    member
  WHERE
    id = {id}
""")
}
