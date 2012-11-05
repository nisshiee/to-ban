package org.nisshiee.toban.model

import scalaz._, Scalaz._
import java.sql.Connection

case class TableRow(date: LocalDate, memoOpt: Option[String], assign: Seq[(Task, Option[Member])])

case class Table(tasks: Seq[Task], rows: Seq[TableRow])

object Table {

  def row(date: LocalDate, tasks: Seq[Task])(implicit c: Connection) = {
    val memoOpt = Memo.find(date) map { _.memo }
    val assign = tasks ∘ { t => Toban.find(t.id, date) ∘ (_.member) |> (t ->) }
    TableRow(date, memoOpt, assign)
  }

  def apply(dates: Seq[LocalDate])(implicit c: Connection): Table = {
    val tasks = Task.all
    val rows = dates ∘ { d => row(d, tasks) }
    Table(tasks, rows)
  }
}
