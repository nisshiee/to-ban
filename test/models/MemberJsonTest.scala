package org.nisshiee.toban.model

import org.specs2._
import org.specs2.matcher.DataTables

import play.api.libs.json.Json._

class MemberJsonTest extends Specification with DataTables { def is =

  "Member.Status"                                                                                     ^
    "Writes"                                                                                          ^
      """Member.StatusオブジェクトをJson.toJsonでJsValueに変換できること
JsValueの型はJsNumberで、Normal = 0, Deleted = 1"""                                                   ! e5^
                                                                                                      p^
    "Reads"                                                                                           ^
      """JsNumberをMember.Statusに変換できること
0 = Normal, 1 = Deleted, その他 = Undefined"""                                                        ! e6^
                                                                                                      p^
                                                                                                      p^
  "Member"                                                                                            ^
    "Writes"                                                                                          ^
      "MemberオブジェクトをJson.toJsonでJsValueに変換できること"                                      ! e1^
                                                                                                      p^
    "Reads"                                                                                           ^
      "JsObject[id->Int,name->String,status->Int]をMemberオブジェクトに変換できること"                ! e2^
      "要素が欠けている場合は変換に失敗すること"                                                      ! e3^
      "要素の型が違う場合は変換に失敗すること"                                                        ! e4^
                                                                                                    end

  def e1 = {
    val member = Member(1, "testmember", Member.Normal)
    val js = toJson(member)

    ((js \ "id").asOpt[Int] must beSome.which(1 ==)) and
    ((js \ "name").asOpt[String] must beSome.which("testmember" ==)) and
    ((js \ "status").asOpt[Member.Status] must beSome.which(Member.Normal ==))
  }

  def e2 = {
    val js = toJson(
      Map(
         "id" -> toJson(1)
        ,"name" -> toJson("testmember")
        ,"status" -> toJson[Member.Status](Member.Normal)
      )
    )
    val member = js.asOpt[Member]

    member must beSome.which(Member(1, "testmember", Member.Normal) ==)
  }

  def e3 = {
    val js = toJson(
      Map(
        "name" -> toJson("testmember")
      )
    )
    val member = js.asOpt[Member]

    member must beNone
  }

  def e4 = {
    val js = toJson(
      Map(
         "id" -> toJson("1")
        ,"name" -> toJson(123)
        ,"status" -> toJson[Member.Status](Member.Normal)
      )
    )
    val member = js.asOpt[Member]

    member must beNone
  }

  def e5 =
    "status"       | "js"      |
    Member.Normal  ! toJson(0) |
    Member.Deleted ! toJson(1) |> { (status: Member.Status, js) =>
      toJson(status) must equalTo(js)
    }

  def e6 =
    "js"       | "status"         |
    toJson(0)  ! Member.Normal    |
    toJson(1)  ! Member.Deleted   |
    toJson(2)  ! Member.Undefined |
    toJson(99) ! Member.Undefined |
    toJson(-1) ! Member.Undefined |> { (js, status) =>
      js.asOpt[Member.Status] must beSome.which(status ==)
    }

}
