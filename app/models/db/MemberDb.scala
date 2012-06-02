package org.nisshiee.toban.model.db

import anorm._, SqlParser._
import org.nisshiee.toban.model._

object MemberDb {

  val parser: RowParser[Member] = int("id") ~ str("name") map {
    case id ~ name => Member(id, name)
  }

  val createSql = SQL("""
INSERT INTO member
  SET
    name = {name}
""")

  val createKeyParser = int("null.SCOPE_IDENTITY()").single

  val allSql = SQL("""
SELECT
    id, name
  FROM
    member
""")

  val findSql = SQL("""
SELECT
    id, name
  FROM
    member
  WHERE
    id = {id}
""")
}
