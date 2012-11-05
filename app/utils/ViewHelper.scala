package org.nisshiee.toban.util

import org.nisshiee.toban.model._

object ViewHelper {

  implicit def member2rich(member: Member): RichMember = RichMember(member)

  case class RichMember(base: Member) {

    def labelClass = (base.status, base.color) match {
      case (Member.Deleted, _) => "label-inverse"
      case (_, Member.Green) => "label-success"
      case (_, Member.Yellow) => "label-warning"
      case (_, Member.Red) => "label-important"
      case _ => "label-info"
    }

    def btnClass = (base.status, base.color) match {
      case (Member.Deleted, _) => "btn-inverse"
      case (_, Member.Green) => "btn-success"
      case (_, Member.Yellow) => "btn-warning"
      case (_, Member.Red) => "btn-danger"
      case _ => "btn-info"
    }
  }
}
