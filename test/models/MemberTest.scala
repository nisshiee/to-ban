package org.nisshiee.toban.model

import org.specs2._
import scalaz._, Scalaz._

class MemberTest extends Specification { def is =

  "Memberケースクラスのテスト"                              ^
    "MemberShowのテスト"                                    ^
      "showsの値はnameと一致(asciiのみ)"                    ! e1^
      "showsの値はnameと一致(日本語を含む)"                 ! e2^
      "nameがnullの場合は空文字"                            ! e3^
                                                            p^
    "MemberEqualのテスト"                                   ^
      "idが一致していればtrue(nameも一致)"                  ! e4^
      "idが一致していればtrue(nameは異なる)"                ! e5^
      "idが異なる場合はfalse(nameも異なる)"                 ! e6^
      "idが異なる場合はfalse(nameは一致)"                   ! e7^
                                                            end

  def e1 = Member(1, "test-member").shows must_== "test-member"
  def e2 = Member(1, "テストメンバ").shows must_== "テストメンバ"
  def e3 = Member(1, null).shows must_== ""

  def e4 = Member(1, "test-member") ≟ Member(1, "test-member") must beTrue
  def e5 = Member(1, "test-member") ≟ Member(1, "other-member") must beTrue
  def e6 = Member(1, "test-member") ≟ Member(2, "other-member") must beFalse
  def e7 = Member(1, "test-member") ≟ Member(2, "test-member") must beFalse
}
