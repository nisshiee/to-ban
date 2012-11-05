package org.nisshiee.toban.model.db

import anorm._, SqlParser._
import org.nisshiee.toban.model._

object MemberDb {

  val parser: RowParser[Member] = int("id") ~ str("name") ~ int("status") ~ int("color") map {
    case id ~ name ~ s ~ c => Member(id, name, Member.Status(s), Member.Color(c))
  }

  val createSql = SQL("""
INSERT INTO member
  SET
    name = {name}
""")

  val createKeyParser = int("null.SCOPE_IDENTITY()").single

  val allSql = SQL("""
SELECT
    id, name, status, color
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
    id, name, status, color
  FROM
    member
  WHERE
    id = {id}
""")

  val deleteSql = {
    val (Member.Status(deleted), Member.Status(normal)) =
      (Member.Deleted, Member.Normal)

    SQL("""
UPDATE
    member
  SET
    status = {deleted}
  WHERE
    id = {id}
    AND status = {normal}
""").on(
       'deleted -> deleted
      ,'normal -> normal
    )
  }

  val updateSql = SQL("""
UPDATE
    member
  SET
    name = {name}
  WHERE
    id = {id}
""")
}
