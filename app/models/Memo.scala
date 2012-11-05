package org.nisshiee.toban.model

import scalaz._, Scalaz._
import java.sql.Connection

import org.nisshiee.toban.model.db.MemoDb._

case class Memo(date: LocalDate, memo: String)

object Memo {

  def find(date: LocalDate)(implicit c: Connection) =
    findSql.on('date -> date.toDate).singleOpt(parser)

  def replace(date: LocalDate, memo: String)(implicit c: Connection) =
    find(date) >| {
      updateSql.on('date -> date.toDate, 'memo -> memo).executeUpdate() match {
        case 1 => Memo(date, memo)
        case _ => new Exception("DB Error")
      }
    } | {
      createSql.on('date -> date.toDate, 'memo -> memo).executeUpdate() match {
        case 1 => Memo(date, memo)
        case _ => new Exception("DB Error")
      }
    }

  def delete(date: LocalDate)(implicit c: Connection) =
    deleteSql.on(
      'date -> date.toDate
    ).executeUpdate() match {
      case 1 => true
      case 0 => false
      case _ => throw new Exception("DB ERROR")
    }
}

trait Memos {

  implicit lazy val MemoEqual = equalBy[Memo, LocalDate](_.date)

  implicit lazy val MemoShow = shows[Memo] {
    case Memo(_, memo) => Option(memo) | ""
  }
}
