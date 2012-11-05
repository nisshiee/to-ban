package org.nisshiee.toban.model

import scalaz._, Scalaz._
import java.sql.Connection

import org.nisshiee.toban.model.db.MemberColorDb._

trait MemberColor {

  sealed trait Color
  case object Blue extends Color
  case object Green extends Color
  case object Yellow extends Color
  case object Red extends Color

  object Color {
    case object Undefined extends Color

    def apply(i: Int) = i match {
      case 0 => Blue
      case 1 => Green
      case 2 => Yellow
      case 3 => Red
      case _ => Undefined
    }

    def unapply(c: Color): Option[Int] = c match {
      case Blue => 0.some
      case Green => 1.some
      case Yellow => 2.some
      case Red => 3.some
      case Undefined => none
    }

    def isValid: Member.Color => Boolean = {
      case Member.Color.Undefined => false
      case _ => true
    }

    sealed trait ChangeError
    case object UnknownMember extends ChangeError
    case object UnknownColor extends ChangeError
  }

  def changeColor(memberId: Int, color: Member.Color)(implicit c: Connection): Validation[Color.ChangeError, Member] =
    color match {
      case Member.Color(cid) => 
        Member.find(memberId) map { m =>
          changeSql.on('id -> m.id, 'color -> cid).executeUpdate()
          m.copy(color = color)
        } toSuccess Color.UnknownMember
      case _ => Color.UnknownColor.fail
    }
      
}
