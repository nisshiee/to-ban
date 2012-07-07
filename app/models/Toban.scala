package org.nisshiee.toban.model

import scalaz._, Scalaz._, Validation.Monad._
import java.sql.Connection

import org.nisshiee.toban.model.db.TobanDb._

case class Toban(task: Task, date: LocalDate, member: Member)

object Toban {

  def find(taskId: Int, date: LocalDate)(implicit c: Connection) = findSql.on(
    'taskId -> taskId
    ,'date -> date.toDate
  ).singleOpt(parser)


  sealed trait ReplaceError
  case object NoTask extends ReplaceError
  case object NoMember extends ReplaceError
  case object DbError extends ReplaceError

  def replace(taskId: Int, date: LocalDate, memberId: Int)(implicit c: Connection) =
    for {
      task <- Task.find(taskId).toSuccess[ReplaceError](NoTask)
      member <- Member.find(memberId).toSuccess[ReplaceError](NoMember)
      toban <- find(taskId, date) match {
        case Some(t) => updateSql.on(
            'taskId -> taskId
            ,'date -> date.toDate
            ,'memberId -> memberId
          ).executeUpdate() match {
          case 1 => Toban(task, date, member).success[ReplaceError]
          case _ => DbError.fail
        }
        case None => createSql.on(
            'taskId -> taskId
            ,'date -> date.toDate
            ,'memberId -> memberId
          ).executeUpdate() match {
          case 1 => Toban(task, date, member).success[ReplaceError]
          case _ => DbError.fail
        }
      }
    } yield toban

  def delete(taskId: Int, date: LocalDate)(implicit c: Connection) =
    deleteSql.on(
       'taskId -> taskId
      ,'date -> date.toDate
    ).executeUpdate() match {
      case 1 => true
      case 0 => false
      case _ => throw new Exception("DB ERROR")
    }
}

