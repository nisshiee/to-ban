package org.nisshiee.toban.model.db

import anorm._, SqlParser._
import org.nisshiee.toban.model._

object MemberColorDb {

  val changeSql = SQL("""
UPDATE
    member
  SET
    color = {color}
  WHERE
    id = {id}
""")
}
