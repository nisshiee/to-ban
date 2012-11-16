package org.nisshiee.toban.util

import org.nisshiee.toban.model._

object ViewHelper {

  implicit def color2rich(color: Member.Color): RichColor = RichColor(color)
  implicit def member2rich(member: Member): RichMember = RichMember(member)

  val colors = List(
     Member.Gold
    ,Member.Silver
    ,Member.Green
    ,Member.Blue
    ,Member.Yellow
    ,Member.Red
    ,Member.Dark
  )

  case class RichColor(base: Member.Color) {

    def btnClass = base match {
      case Member.Green => "btn-success"
      case Member.Yellow => "btn-warning"
      case Member.Red => "btn-danger"
      case Member.Dark => "btn-dark"
      case Member.Silver => "btn-silver"
      case Member.Gold => "btn-gold"
      case _ => "btn-info"
    }

    def changeFormId = base match {
      case Member.Green => "change-green"
      case Member.Yellow => "change-yellow"
      case Member.Red => "change-red"
      case Member.Dark => "change-dark"
      case Member.Silver => "change-silver"
      case Member.Gold => "change-gold"
      case _ => "change-blue"
    }
  }

  case class RichMember(base: Member) {

    def labelClass = (base.status, base.color) match {
      case (Member.Deleted, _) => "label-inverse"
      case (_, Member.Green) => "label-success"
      case (_, Member.Yellow) => "label-warning"
      case (_, Member.Red) => "label-important"
      case (_, Member.Dark) => "label-dark"
      case (_, Member.Silver) => "label-silver"
      case (_, Member.Gold) => "label-gold"
      case _ => "label-info"
    }

    def btnClass = base.status match {
      case Member.Deleted => "btn-inverse"
      case _ => base.color.btnClass
    }
  }
}
