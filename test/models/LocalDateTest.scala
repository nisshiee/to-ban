package org.nisshiee.toban.model

import org.specs2._

import scalaz._, Scalaz._

class LocalDateTest extends Specification { def is =

  "LocalDate2Rich"                                                              ^
    "implicit conversionできること"                                             ! e1^
                                                                                p^
  "dayOfWeekType"                                                               ^
    "2012-06-10はSunday"                                                        ! e2^
    "2012-06-11はBizDay"                                                        ! e3^
    "2012-06-13はBizDay"                                                        ! e4^
    "2012-06-15はBizDay"                                                        ! e5^
    "2012-06-16はSaturday"                                                      ! e6^
                                                                                end

  def e1 = {
    val r: MyRichLocalDate = LocalDate.today
    // コンパイルが通ればOK
    success
  }

  def e2 = new LocalDate("2012-06-10").dayOfWeekType must equalTo(Sunday)
  def e3 = new LocalDate("2012-06-11").dayOfWeekType must equalTo(Bizday)
  def e4 = new LocalDate("2012-06-13").dayOfWeekType must equalTo(Bizday)
  def e5 = new LocalDate("2012-06-15").dayOfWeekType must equalTo(Bizday)
  def e6 = new LocalDate("2012-06-16").dayOfWeekType must equalTo(Saturday)
}
