package org.nisshiee.toban.model.db

import anorm._, SqlParser._
import org.nisshiee.toban.model._

object TobanDb {

  val parser =
    int("task.id") ~
    str("task.name") ~
    date("toban.date") ~
    int("member.id") ~
    str("member.name") ~
    int("member.status") ~
    int("member.color") map {
      case taskId ~ taskName ~ date ~ memberId ~ memberName ~ s ~ c =>
        Toban(
           Task(taskId, taskName)
          ,date.toLocalDate
          ,Member(memberId, memberName, Member.Status(s), Member.Color(c))
        )
    }

  val findSql = SQL("""
SELECT
    task.id
    ,task.name
    ,toban.date
    ,member.id
    ,member.name
    ,member.status
    ,member.color
  FROM
    toban
    ,task
    ,member
  WHERE
    toban.task_id = {taskId}
    AND toban.date = {date}
    AND toban.task_id = task.id
    AND toban.member_id = member.id
""")

  val createSql = SQL("""
INSERT INTO toban
  SET
    task_id = {taskId}
    ,date = {date}
    ,member_id = {memberId}
""")

  val updateSql = SQL("""
UPDATE
    toban
  SET
    member_id = {memberId}
  WHERE
    task_id = {taskId}
    AND date = {date}
""")

  val deleteSql = SQL("""
DELETE
    toban
  WHERE
    task_id = {taskId}
    AND date = {date}
""")
}
