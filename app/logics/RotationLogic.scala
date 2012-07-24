package org.nisshiee.toban.logic

import scalaz._, Scalaz._
import org.nisshiee.toban.model._

object RotationLogic {

  case class OperationResult(
     result: Seq[Rotation]
    ,inserted: Seq[Rotation]
    ,updated: Seq[Rotation]
    ,deleted: Seq[Int]
  )

  sealed trait DeleteError
  case object NoId extends DeleteError with MoveError

  def delete(
    before: Seq[Rotation])(
    id: Int)(
    implicit maxScore: Int, minScore: Int): Validation[DeleteError, OperationResult] =
    OperationResult(Seq(), Seq(), Seq(), Seq()).success

  sealed trait InsertError
  case object Full extends InsertError
  case object IllegalPos extends InsertError with MoveError

  def insert(
    before: Seq[Rotation])(
    task: Task, member: Member, pos: Int)(
    implicit maxScore: Int, minScore: Int): Validation[InsertError, OperationResult] =
      OperationResult(Seq(), Seq(), Seq(), Seq()).success

  sealed trait MoveError

  def move(
    before: Seq[Rotation])(
    id: Int, pos: Int)(
    implicit maxScore: Int, minScore: Int): Validation[MoveError, OperationResult] =
      OperationResult(Seq(), Seq(), Seq(), Seq()).success
}
