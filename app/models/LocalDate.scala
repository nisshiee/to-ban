package org.nisshiee.toban.model

import scalaz._, Scalaz._

trait LocalDates {

  implicit lazy val LocalDateEqual = equalA[LocalDate]

  implicit lazy val LocalDateShow = showA[LocalDate]

  implicit def LocalDate2Rich: LocalDate => MyRichLocalDate = MyRichLocalDate.apply
}

case class MyRichLocalDate(value: LocalDate) {
  
  def dayOfWeekType = value.dayOfWeek.get match {
    case 6 => Saturday
    case 7 => Sunday
    case _ => Bizday
  }
}

sealed trait DayOfWeekType
case object Sunday extends DayOfWeekType
case object Saturday extends DayOfWeekType
case object Bizday extends DayOfWeekType
