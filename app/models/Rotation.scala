// ★★★ 仮 実 装 ★★★

package org.nisshiee.toban.model

sealed trait Rotation {
  val task: Task
  val member: Member
  val score: Int
  val status: Rotation.Status
}
case class IdentifiedRotation(
  id: Int, task: Task, member: Member, score: Int, status: Rotation.Status
) extends Rotation
case class UnIdentifiedRotation(
  task: Task, member: Member, score: Int, status: Rotation.Status
) extends Rotation

object Rotation {

  sealed trait Status
  case object On extends Status
  case object Off extends Status

  case class Env(scoreMax: Int, scoreMin: Int)
}

trait Rotations {

  implicit val DefaultEnv = Rotation.Env(Int.MaxValue, 0)
}
