package org.nisshiee.toban.model

import org.specs2._
import play.api.test._, Helpers._
import play.api.Play.current

import scalaz._, Scalaz._
import play.api.db._

class MemberTest extends Specification { def is =

  "Memberケースクラスのテスト"                                                  ^
    "MemberShowのテスト"                                                        ^
      "showsの値はnameと一致(asciiのみ)"                                        ! e1^
      "showsの値はnameと一致(日本語を含む)"                                     ! e2^
      "nameがnullの場合は空文字"                                                ! e3^
                                                                                p^
    "MemberEqualのテスト"                                                       ^
      "idが一致していればtrue(nameも一致)"                                      ! e4^
      "idが一致していればtrue(nameは異なる)"                                    ! e5^
      "idが異なる場合はfalse(nameも異なる)"                                     ! e6^
      "idが異なる場合はfalse(nameは一致)"                                       ! e7^
                                                                                p^
                                                                                p^
  "CRUDテスト"                                                                  ^
    "allのテスト"                                                               ^
      "何もCreateしてなければ空"                                                ! e8^
                                                                                p^
    "createのテスト"                                                            ^
      "createに成功するとSome[Member]が返る"                                    ! e9^
      "2回createに成功すると共にSome[Member]が返り、IDは異なる"                 ! e13^
                                                                                p^
    "create→allのテスト"                                                       ^
      "1回createすると、allでそのMemberが返る"                                  ! e10^
      "2回createすると、allで2Memberが返り、idはそれぞれ異なる"                 ! e11^
      "日本語名もcreateできて、allで取得しても化けない"                         ! e12^
                                                                                p^
    "findのテスト"                                                              ^
      "存在しないIDに対するfindはNoneが返る"                                    ! e14^
                                                                                p^
    "create→findのテスト"                                                      ^
      "1回createし、そのIDでfindすると対象レコードがSome[Member]で返る"         ! e15^
                                                                                end

  def e1 = Member(1, "test-member").shows must_== "test-member"
  def e2 = Member(1, "テストメンバ").shows must_== "テストメンバ"
  def e3 = Member(1, null).shows must_== ""

  def e4 = Member(1, "test-member") ≟ Member(1, "test-member") must beTrue
  def e5 = Member(1, "test-member") ≟ Member(1, "other-member") must beTrue
  def e6 = Member(1, "test-member") ≟ Member(2, "other-member") must beFalse
  def e7 = Member(1, "test-member") ≟ Member(2, "test-member") must beFalse
  
  def e8 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      Member.all must be empty
    }
  }

  def e9 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val name = "testmember"
      Member.create(name) must beSome.which(_.name ≟ name)
    }
  }

  def e13 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val name1 = "testmember1"
      val name2 = "testmember2"
      val createResult1 = Member.create(name1)
      val createResult2 = Member.create(name2)

      val validation = for {
        member1 <- createResult1
        member2 <- createResult2
        idCheck = member1.id ≠ member2.id
        nameCheck1 = name1 ≟ member1.name
        nameCheck2 = name2 ≟ member2.name
      } yield (idCheck && nameCheck1 && nameCheck2)
      validation must beSome.which(identity)
    }
  }

  def e10 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val name = "testmember"
      val createResult = Member.create(name)
      val all = Member.all

      val spec1 = all must have size(1)
      val spec2 = all ∘ {
        case m @ Member(_, n) => (n ≟ name) && (m.some ≟ createResult)
      } must_== List(true)

      spec1 and spec2
    }
  }


  def e11 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val (name1, name2) = ("testmember1", "testmember2")
      val createResult1 = Member.create(name1)
      val createResult2 = Member.create(name2)
      val ids = List(createResult1, createResult2).flatten ∘ (_.id)
      val all = Member.all

      val spec1 = all ∘ (_.name) must contain(name1, name2).only
      val spec2 = all ∘ (_.id) must containAllOf(ids).only

      spec1 and spec2
    }
  }

  def e12 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val name = "日本語メンバー"
      val createResult = Member.create(name)
      val all = Member.all

      val spec1 = all must have size(1)
      val spec2 = all ∘ {
        case m @ Member(_, n) => (n ≟ name) && (m.some ≟ createResult)
      } must_== List(true)

      spec1 and spec2
    }
  }

  def e14 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      Member.find(1) must beNone
    }
  }

  def e15 = running(FakeApplication()) {
    DB.withTransaction { implicit c =>
      val validation = for {
        t <- Member.create("testtask")
        found <- Member.find(t.id)
      } yield ((t.id ≟ found.id) && (t.name ≟ found.name))
      validation must beSome.which(identity)
    }
  }
}