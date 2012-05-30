package org.nisshiee.toban.model.db

import anorm._, SqlParser._
import org.nisshiee.toban.model._

object TaskDb {

  val parser: RowParser[Task] = int("id") ~ str("name") map {
    case id ~ name => Task(id, name)
  }

  val createSql = SQL("""
INSERT INTO task
  SET
    name = {name}
""")

  val createKeyParser = int("null.SCOPE_IDENTITY()").single

  val allSql = SQL("""
SELECT
    id, name
  FROM
    task
""")
}
