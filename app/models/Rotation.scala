// ★★★ 仮 実 装 ★★★

package org.nisshiee.toban.model

case class Rotation(id: Int, task: Task, member: Member, score: Int, status: Rotation.Status)

object Rotation {

  sealed trait Status
  case object On extends Status
  case object Off extends Status

  implicit val ScoreMax = Int.MaxValue
  implicit val ScoreMin = Int.MinValue
}
