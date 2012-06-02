package org.nisshiee.toban.model

import scalaz._, Scalaz._

trait LocalDates {

  implicit lazy val LocalDateEqual = equalA[LocalDate]

  implicit lazy val LocalDateShow = showA[LocalDate]
}
