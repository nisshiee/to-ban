package org.nisshiee.toban.model

import org.specs2._, matcher.DataTables
import play.api.test._, Helpers._
import play.api.Play.current

import scalaz._, Scalaz._, Validation.Monad._
import play.api.db._

import org.nisshiee.toban.test.TestHelper

class MemberTest extends Specification with TestHelper { def is =

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
    "create→changeColor→find→delete→find→deleteのテスト"                   ^
      """create→成功
changeColor→成功
find→createしたmemberがSome[Member]で返る
delete→deleteに成功し、statusが更新されたmemberがSome[Member]で返る
find→statusがDeletedになったmemberがSome[Member]で返る
delete→既にstatusがNormalでないのでInvalidStatusが返る"""                      ! e15^
                                                                                p^
    "deleteのテスト"                                                            ^
      "存在しないIDに対するdeleteはNoMemberがFailureで返る"                     ! e16^
      "statusがNormalでないmemberに対するdeleteはInvalidStatusがFailureで返る"  ! e17^
      """statusがNormalであるmemberに対するdeleteは
statusが更新されたMemberがSuccessで返る"""                                      ! e18^
                                                                                p^
    "updateのテスト"                                                            ^
      "存在しないIDに対するupdateはNoneが返る"                                  ! e19^
      "updateに成功するとはSome[Member]が返る"                                  ! e20^
                                                                                end

  def e1 = Member(1, "test-member", Member.Normal).shows must_== "test-member"
  def e2 = Member(1, "テストメンバ", Member.Normal).shows must_== "テストメンバ"
  def e3 = Member(1, null, Member.Normal).shows must_== ""

  def e4 =
    Member(1, "test-member", Member.Normal) ≟ Member(1, "test-member", Member.Normal) must beTrue
  def e5 =
    Member(1, "test-member", Member.Normal) ≟ Member(1, "other-member", Member.Normal) must beTrue
  def e6 =
    Member(1, "test-member", Member.Normal) ≟ Member(2, "other-member", Member.Normal) must beFalse
  def e7 =
    Member(1, "test-member", Member.Normal) ≟ Member(2, "test-member", Member.Normal) must beFalse
  
  def e8 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      Member.all must be empty
    }
  }

  def e9 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      val name = "testmember"
      Member.create(name) must beSome.which(_.name ≟ name)
    }
  }

  def e13 = runningEmptyApplication {
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

  def e10 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      val name = "testmember"
      val createResult = Member.create(name)
      val all = Member.all

      val spec1 = all must have size(1)
      val spec2 = all ∘ {
        case m @ Member(_, n, _, _) => (n ≟ name) && (m.some ≟ createResult)
      } must_== List(true)

      spec1 and spec2
    }
  }


  def e11 = runningEmptyApplication {
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

  def e12 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      val name = "日本語メンバー"
      val createResult = Member.create(name)
      val all = Member.all

      val spec1 = all must have size(1)
      val spec2 = all ∘ {
        case m @ Member(_, n, _, _) => (n ≟ name) && (m.some ≟ createResult)
      } must_== List(true)

      spec1 and spec2
    }
  }

  def e14 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      Member.find(1) must beNone
    }
  }

  def e15 = runningEmptyApplication {
    DB.withTransaction { implicit c =>
      val validation = for {
        m1 <- Member.create("testmember")
        m1_1 <- Member.changeColor(m1.id, Member.Green).toOption
        m2 <- Member.find(m1.id)
        m3 <- Member.delete(m1.id).toOption
        m4 <- Member.find(m1.id)
        m5Vld = Member.delete(m1.id)
      } yield (
        (m1.id ≟ m2.id) &&
        (m1.name ≟ m2.name) &&
        (m2.status == Member.Normal) &&
        (m2.color == Member.Green) &&
        (m1.id ≟ m3.id) &&
        (m3.status == Member.Deleted) &&
        (m1.id ≟ m4.id) &&
        (m4.status == Member.Deleted) &&
        (m5Vld == Member.InvalidStatus(Member.Deleted).fail[Member])
      )
      validation must beSome.which(identity)
    }
  }

  def e16 = runningEmptyApplication {
    DB.withConnection { implicit c =>
      Member.delete(1) must equalTo(Member.NoMember.fail[Member])
    }
  }

  def e17 = runningEmptyApplication {
    DB.withConnection { implicit c =>
      for {
        member <- Member.create("testmember")
        id = member.id
        _ <- Member.delete(id).toOption
      } yield Member.delete(id) == Member.InvalidStatus(Member.Deleted).fail[Member]
    } must beSome.which(identity)
  }

  def e18 = runningEmptyApplication {
    DB.withConnection { implicit c =>
      for {
        before <- Member.create("testmember")
        id = before.id
        after <- Member.delete(id).toOption
      } yield after
    } must beSome.which { m =>
      m.status == Member.Deleted
    }
  }

  def e19 = runningEmptyApplication {

    DB.withConnection { implicit c =>
      Member.update(1, "updatedmember")
    } must beNone
  }

  def e20 = runningEmptyApplication {

    val newname = "updatedmember"

    DB.withConnection { implicit c =>
      for {
        before <- Member.create("testmember")
        id = before.id
        after <- Member.update(id, newname)
      } yield after
    } must beSome.which { m =>
      m.name == newname
    }
  }
}
