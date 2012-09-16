package org.nisshiee.toban.model.db

import anorm._, SqlParser._
import org.nisshiee.toban.model._

object MemoDb {

  val parser: RowParser[Memo] = date("date") ~ str("memo") map {
    case date ~ memo => Memo(date.toLocalDate, memo)
  }

  val createSql = SQL("""
INSERT INTO memo
  SET
    date = {date}
    ,memo = {memo}
""")

  val findSql = SQL("""
SELECT
    date, memo
  FROM
    memo
  WHERE
    date = {date}
""")

  val updateSql = SQL("""
UPDATE
    memo
  SET
    memo = {memo}
  WHERE
    date = {date}
""")

  val deleteSql = SQL("""
DELETE
    memo
  WHERE
    date = {date}
""")
}
