package org.nisshiee.toban.model

import org.specs2._
import play.api.test._, Helpers._
import play.api.Play.current

import scalaz._, Scalaz._
import play.api.db._

import org.nisshiee.toban.test.TestHelper

class MemoTest extends Specification with TestHelper { def is =

  "Memoケースクラスのテスト"                                                    ^
    "MemoShowのテスト"                                                          ^
      "showsの値はmemoと一致(asciiのみ)"                                        ! e1^
      "showsの値はmemoと一致(日本語を含む)"                                     ! e2^
      "memoがnullの場合は空文字"                                                ! e3^
                                                                                p^
    "MemoEqualのテスト"                                                         ^
      "dateが一致していればtrue(memoも一致)"                                    ! e4^
      "dateが一致していればtrue(memoは異なる)"                                  ! e5^
      "dateが異なる場合はfalse(memoも異なる)"                                   ! e6^
      "dateが異なる場合はfalse(memoは一致)"                                     ! e7^
                                                                                p^
                                                                                p^
  "CRUDテスト"                                                                  ^
    "findのテスト"                                                              ^
      "存在しないdateに対するfindはNoneが返る"                                  ! e8^
                                                                                p^
    "replaceのテスト"                                                           ^
      "dateが一致するレコードがなければcreateされ、Memoが返る"                  ! e9^
      "dateが一致するレコードがあればUpdateされ、Memoが返る"                    ! e10^
                                                                                p^
    "replace→findのテスト"                                                     ^
      "新規create後、そのレコードをfindできる"                                  ! e11^
      "update後、そのレコードをfindすると更新後のMemoを取得"                    ! e12^
                                                                                p^
    "deleteのテスト"                                                            ^
      "dateが一致するレコードがなければfalseが返る"                             ! e13^
      "dateが一致するレコードがあればDeleteされ、trueが返る"                    ! e14^
                                                                                p^
    "delete→findのテスト"                                                      ^
      "delete後、そのレコードをfindするとNoneが返る"                            ! e15^
                                                                                end

  def e1 = Memo(LocalDate.today, "test-memo").shows must_== "test-memo"
  def e2 = Memo(LocalDate.today, "テストメモ").shows must_== "テストメモ"
  def e3 = Memo(LocalDate.today, null).shows must_== ""

  def e4 = Memo(LocalDate.today, "test-memo") ≟ Memo(LocalDate.today, "test-memo") must beTrue
  def e5 = Memo(LocalDate.today, "test-memo") ≟ Memo(LocalDate.today, "other-memo") must beTrue
  def e6 = Memo(LocalDate.today, "test-memo") ≟ Memo(LocalDate.yesterday, "other-memo") must beFalse
  def e7 = Memo(LocalDate.today, "test-memo") ≟ Memo(LocalDate.yesterday, "test-memo") must beFalse

  def e8 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      Memo.find(LocalDate.today) must beNone
    }
  }

  def e9 = runningEmptyApplication {
    val date = LocalDate.today
    val memo = "test-memo"
    DB.withTransaction { implicit c =>
      Memo.replace(date, memo) must equalTo(Memo(date, memo))
    }
  }

  def e10 = runningEmptyApplication {
    val date = LocalDate.today
    val memo1 = "test-memo"
    val memo2 = "new-memo"
    val result = DB.withTransaction { implicit c =>
      Memo.replace(date, memo1)
      Memo.replace(date, memo2)
    }
    result must equalTo(Memo(date, memo2))
  }

  def e11 = runningEmptyApplication {
    val date = LocalDate.today
    val memo = "test-memo"
    val resultOpt = DB.withTransaction { implicit c =>
      Memo.replace(date, memo)
      Memo.find(date)
    }
    resultOpt must beSome.which(Memo(date, memo) ==)
  }

  def e12 = runningEmptyApplication {
    val date = LocalDate.today
    val memo1 = "test-memo"
    val memo2 = "new-memo"
    val resultOpt = DB.withTransaction { implicit c =>
      Memo.replace(date, memo1)
      Memo.replace(date, memo2)
      Memo.find(date)
    }
    resultOpt must beSome.which(Memo(date, memo2) ==)
  }

  def e13 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      Memo.delete(LocalDate.today)
    } must beFalse
  }

  def e14 = runningEmptyApplication {
    val date = LocalDate.today
    val memo = "test-memo"
    val resultOpt = DB.withTransaction { implicit c =>
      Memo.replace(date, memo)
      Memo.delete(date)
    }
    resultOpt must beTrue
  }

  def e15 = runningEmptyApplication {
    val date = LocalDate.today
    val memo = "test-memo"
    val result = DB.withTransaction { implicit c =>
      Memo.replace(date, memo)
      Memo.delete(date)
      Memo.find(date)
    }
    result must beNone
  }

}
