package org.nisshiee.toban.model

import scalaz._, Scalaz._

trait LocalDates {

  def str2DateOpt(str: String) = {
    val YMD = """(\d{4}-\d{2}-\d{2})""".r
    str match {
      case YMD(s) => try { new LocalDate(s).some } catch { case _ => None }
      case _ => None
    }
  }

  implicit lazy val LocalDateEqual = equalA[LocalDate]

  implicit lazy val LocalDateShow = showA[LocalDate]

  implicit def LocalDate2Rich: LocalDate => MyRichLocalDate = MyRichLocalDate.apply

  import play.api.libs.json._, Json._

  implicit lazy val LocalDateWrites = new Writes[LocalDate] {
    def writes(date: LocalDate) = toJson(date.toString)
  }

  implicit lazy val LocalDateReads = new Reads[LocalDate] {
    def reads(js: JsValue) = str2DateOpt(js.as[String]) | (throw new RuntimeException("Wrong Format"))
  }
}

case class MyRichLocalDate(value: LocalDate) {
  
  def dayOfWeekType = value.dayOfWeek.get match {
    case 6 => Saturday
    case 7 => Sunday
    case _ => Bizday
  }

  def weeklyList = {
    val start = value - (value.dayOfWeek.get % 7).day
    (0 until 7).toList ∘ (_.day) ∘ (start +)
  }

  def isToday = LocalDate.today ≟ value
}

sealed trait DayOfWeekType
case object Sunday extends DayOfWeekType
case object Saturday extends DayOfWeekType
case object Bizday extends DayOfWeekType
