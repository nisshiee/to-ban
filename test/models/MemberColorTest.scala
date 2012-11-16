package org.nisshiee.toban.model

import org.specs2._, matcher.DataTables
import play.api.test._, Helpers._
import play.api.Play.current
import play.api.db._

import scalaz._, Scalaz._

import org.nisshiee.toban.test.TestHelper

class MemberColorTest extends Specification with DataTables with TestHelper { def is =

  "Member.Colorテスト"                                                         ^
    "applyのテスト"                                                             ^
      "0 => Blue"                                                               ! e1^
      "1 => Green"                                                              ! e2^
      "2 => Yellow"                                                             ! e3^
      "3 => Red"                                                                ! e4^
      "4-6 => Dark, Silver, Gold"                                               ! e4_1^
      "それ以外 => UndefinedColor"                                              ! e5^
                                                                                p^
    "unapplyのテスト"                                                           ^
      "Blue => Some(0)"                                                         ! e6^
      "Green => Some(1)"                                                        ! e7^
      "Yellow => Some(2)"                                                       ! e8^
      "Red => Some(3)"                                                          ! e9^
      "Dark, Silver, Gold => Some(4), Some(5), Some(6)"                         ! e9_1^
      "Undefined => None"                                                       ! e10^
      "パターンマッチで使えること"                                              ! e11^
                                                                                p^
    "changeColor"                                                               ^
      "memberIdが存在しない場合はUnknownMemberがfailureで返る"                  ! e12^
      "Undefinedが指定された場合はUndefinedColorがfailureで返る"                ! e13^
      "それ以外の場合は変更後のMemberがSuccessが返る"                           ! e14^
                                                                                p^
    "isValid"                                                                   ^
      "Undefinedならばfalse、それ以外ならばtrueが返る"                          ! e15^
                                                                                end

  def e1 = Member.Color(0) must equalTo(Member.Blue)
  def e2 = Member.Color(1) must equalTo(Member.Green)
  def e3 = Member.Color(2) must equalTo(Member.Yellow)
  def e4 = Member.Color(3) must equalTo(Member.Red)

  def e4_1 =
    "code" | "color"       |
    4      ! Member.Dark   |
    5      ! Member.Silver |
    6      ! Member.Gold   |> { (code, color) =>
      Member.Color(code) must equalTo(color)
    }

  def e5 =
    "in" | "result"               |
    7    ! Member.Color.Undefined |
    99   ! Member.Color.Undefined |
    -1   ! Member.Color.Undefined |> {
      (in, result) => Member.Color(in) must equalTo(result)
    }

  def e6 = Member.Color.unapply(Member.Blue) must equalTo(0.some)
  def e7 = Member.Color.unapply(Member.Green) must equalTo(1.some)
  def e8 = Member.Color.unapply(Member.Yellow) must equalTo(2.some)
  def e9 = Member.Color.unapply(Member.Red) must equalTo(3.some)

  def e9_1 =
    "color"       | "code" |
    Member.Dark   ! 4      |
    Member.Silver ! 5      |
    Member.Gold   ! 6      |> { (color, code) =>
      Member.Color.unapply(color) must beSome.which(code ==)
    }

  def e10 = Member.Color.unapply(Member.Color.Undefined) must beNone
  def e11 = {
    val Member.Color(i) = Member.Blue
    i must equalTo(0)
  }

  def e12 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      Member.changeColor(1, Member.Blue).either must beLeft.like {
        case Member.Color.UnknownMember => ok
      }
    }
  }

  def e13 = runningEmptyApplication {
    val resultOpt = DB.withTransaction { implicit c =>
      for { 
        member <- Member.create("testmember")
      } yield Member.changeColor(member.id, Member.Color.Undefined)
    }
    resultOpt must beSome.like {
      case Failure(Member.Color.UnknownColor) => ok
    }
  }

  def e14 = runningEmptyApplication {
    val actualOpt = DB.withTransaction { implicit c =>
      for {
        member <- Member.create("testmember")
        resultVld = Member.changeColor(member.id, Member.Green)
        found <- Member.find(member.id)
      } yield (resultVld, found)
    }
    actualOpt must beSome.like {
      case (resultVld, found) =>
        (resultVld.either must beRight.like {
          case Member(_, n, _, c) => (n, c) must equalTo("testmember" -> Member.Green)
        }) and
        (found must beLike {
          case Member(_, n, _, c) => (n, c) must equalTo("testmember" -> Member.Green)
        })
    }
  }

  def e15 =
    "color"                | "result" |
    Member.Blue            ! true     |
    Member.Green           ! true     |
    Member.Yellow          ! true     |
    Member.Red             ! true     |
    Member.Color.Undefined ! false    |> { (color, result) =>
      Member.Color.isValid(color) must equalTo(result)
    }
}
