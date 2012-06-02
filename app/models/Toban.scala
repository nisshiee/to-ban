package org.nisshiee.toban.model

import scalaz._, Scalaz._
import java.sql.Connection

import org.nisshiee.toban.model.db.TobanDb._

case class Toban(task: Task, date: LocalDate, member: Member)

object Toban {

  def find(taskId: Int, date: LocalDate)(implicit c: Connection) = findSql.on(
    'taskId -> taskId
    ,'date -> date.toDate
  ).singleOpt(parser)

  def createOrUpdate(taskId: Int, date: LocalDate, memberId: Int)(implicit c: Connection) =
    for {
      task <- Task.find(taskId)
      member <- Member.find(memberId)
      toban <- find(taskId, date) match {
        case Some(t) => updateSql.on(
            'taskId -> taskId
            ,'date -> date.toDate
            ,'memberId -> memberId
          ).executeUpdate() match {
          case 1 => Toban(task, date, member).some
          case _ => none
        }
        case None => createSql.on(
            'taskId -> taskId
            ,'date -> date.toDate
            ,'memberId -> memberId
          ).executeUpdate() match {
          case 1 => Toban(task, date, member).some
          case _ => none
        }
      }
    } yield toban
}

