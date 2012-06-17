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
                                                                                p^
  "weeklyList"                                                                  ^
    "2012-06-09週は3日-9日"                                                     ! e7^
    "2012-06-10週は10日-16日"                                                   ! e8^
    "2012-06-13週は10日-16日"                                                   ! e9^
    "2012-06-16週は10日-16日"                                                   ! e10^
    "2012-06-17週は17日-23日"                                                   ! e11^
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

  def e7 = new LocalDate("2012-06-09").weeklyList must_== List(
    new LocalDate("2012-06-03")
    ,new LocalDate("2012-06-04")
    ,new LocalDate("2012-06-05")
    ,new LocalDate("2012-06-06")
    ,new LocalDate("2012-06-07")
    ,new LocalDate("2012-06-08")
    ,new LocalDate("2012-06-09")
  )

  def e8 = new LocalDate("2012-06-10").weeklyList must_== List(
    new LocalDate("2012-06-10")
    ,new LocalDate("2012-06-11")
    ,new LocalDate("2012-06-12")
    ,new LocalDate("2012-06-13")
    ,new LocalDate("2012-06-14")
    ,new LocalDate("2012-06-15")
    ,new LocalDate("2012-06-16")
  )

  def e9 = new LocalDate("2012-06-13").weeklyList must_== List(
    new LocalDate("2012-06-10")
    ,new LocalDate("2012-06-11")
    ,new LocalDate("2012-06-12")
    ,new LocalDate("2012-06-13")
    ,new LocalDate("2012-06-14")
    ,new LocalDate("2012-06-15")
    ,new LocalDate("2012-06-16")
  )

  def e10 = new LocalDate("2012-06-16").weeklyList must_== List(
    new LocalDate("2012-06-10")
    ,new LocalDate("2012-06-11")
    ,new LocalDate("2012-06-12")
    ,new LocalDate("2012-06-13")
    ,new LocalDate("2012-06-14")
    ,new LocalDate("2012-06-15")
    ,new LocalDate("2012-06-16")
  )

  def e11 = new LocalDate("2012-06-17").weeklyList must_== List(
    new LocalDate("2012-06-17")
    ,new LocalDate("2012-06-18")
    ,new LocalDate("2012-06-19")
    ,new LocalDate("2012-06-20")
    ,new LocalDate("2012-06-21")
    ,new LocalDate("2012-06-22")
    ,new LocalDate("2012-06-23")
  )
}
