// ★★★ 仮 実 装 ★★★

package org.nisshiee.toban.model

case class Rotation(id: Int, task: Task, member: Member, score: Int, status: Rotation.Status)

object Rotation {

  sealed trait Status
  case object On extends Status
  case object Off extends Status

  case class Env(scoreMax: Int, scoreMin: Int)
}

trait Rotations {

  implicit val DefaultEnv = Rotation.Env(Int.MaxValue, 0)
}
