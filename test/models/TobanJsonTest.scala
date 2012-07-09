package org.nisshiee.toban.model

import org.specs2._
import org.specs2.matcher.DataTables

import scalaz._, Scalaz._
import play.api.libs.json.Json._

class TobanJsonTest extends Specification with DataTables { def is =

  "Writes"                                                                      ^
    "TobanオブジェクトをJson.toJsonでJsValueに変換できること"                   ! e1^
                                                                                p^
  "Reads"                                                                       ^
    "JsValueをTobanオブジェクトに変換できること"                                ! e2^
    "型が違う場合は変換に失敗すること"                                          ! e3^
    "日付文字列の形式が不正な場合は変換に失敗すること"                          ! e4^
                                                                                end

  def e1 = {
    val task = Task(1, "testtask")
    val date = LocalDate.today
    val member = Member(2, "testmember", Member.Normal)
    val toban = Toban(task, date, member)
    val js = toJson(toban)
    ((js \ "task").asOpt[Task] must beSome.which(task ≟)) &&
    ((js \ "date").asOpt[LocalDate] must beSome.which(date ≟)) &&
    ((js \ "member").asOpt[Member] must beSome.which(member ≟))
  }

  def e2 = {
    val js = toJson(Map(
       "task" -> toJson(Map(
         "id" -> toJson(1)
        ,"name" -> toJson("testtask")
      ))
      ,"date" -> toJson("2012-01-01")
      ,"member" -> toJson(Map(
         "id" -> toJson(2)
        ,"name" -> toJson("taskmember")
        ,"status" -> toJson(0)
      ))
    ))
    js.asOpt[Toban] must beSome.which(
      Toban(
         Task(1, "testtask")
        ,new LocalDate("2012-01-01")
        ,Member(2, "testmember", Member.Normal)
      ) ≟)
  }

  def e3 = {
    val js = toJson(Map(
       "task" -> toJson(Map(
         "id" -> toJson(1)
        ,"name" -> toJson(3)
      ))
      ,"date" -> toJson("2012-01-01")
      ,"member" -> toJson(Map(
         "id" -> toJson("2")
        ,"name" -> toJson("taskmember")
        ,"status" -> toJson(0)
      ))
    ))
    js.asOpt[Toban] must beNone
  }

  def e4 = {
    val js = toJson(Map(
       "task" -> toJson(Map(
         "id" -> toJson(1)
        ,"name" -> toJson("testtask")
      ))
      ,"date" -> toJson("2012-02-30")
      ,"member" -> toJson(Map(
         "id" -> toJson(2)
        ,"name" -> toJson("taskmember")
        ,"status" -> toJson(0)
      ))
    ))
    js.asOpt[Toban] must beNone
  }
}
