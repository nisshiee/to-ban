package org.nisshiee.toban.model

import org.specs2._, matcher.DataTables
import play.api.test._, Helpers._
import play.api.Play.current

import scalaz._, Scalaz._

class MemberStatusTest extends Specification with DataTables { def is =

  "Member.Statusテスト"                                                         ^
    "applyのテスト"                                                             ^
      "0 => Normal"                                                             ! e1^
      "1 => Deleted"                                                            ! e2^
      "それ以外 => Undefined"                                                   ! e3^
                                                                                p^
    "unapplyのテスト"                                                           ^
      "Normal => Some(0)"                                                       ! e4^
      "Deleted => Some(1)"                                                      ! e5^
      "Undefined => None"                                                       ! e6^
      "パターンマッチで使えること"                                              ! e7^
                                                                                p^
    "MemberStatusShowのテスト"                                                  ^
      "Normal => normal"                                                        ! e8^
      "Deleted => deleted"                                                      ! e9^
      "Undefined => -"                                                          ! e10^
                                                                                end

  def e1 = Member.Status(0) must equalTo(Member.Normal)
  def e2 = Member.Status(1) must equalTo(Member.Deleted)

  def e3 =
    "in" | "result"         |
    2    ! Member.Undefined |
    99   ! Member.Undefined |
    -1   ! Member.Undefined |> {
      (in, result) => Member.Status(in) must equalTo(result)
    }

  def e4 = Member.Status.unapply(Member.Normal) must equalTo(0.some)
  def e5 = Member.Status.unapply(Member.Deleted) must equalTo(1.some)
  def e6 = Member.Status.unapply(Member.Undefined) must beNone
  def e7 = {
    val Member.Status(i) = Member.Normal
    i must equalTo(0)
  }

  def e8 = {
    val s: Member.Status = Member.Normal
    s.shows must equalTo("normal")
  }
  def e9 = (Member.Deleted: Member.Status).shows must equalTo("deleted")
  def e10 = (Member.Undefined: Member.Status).shows must equalTo("-")
}
