package org.nisshiee.toban.model

import org.specs2._

import play.api.libs.json.Json._

class TaskJsonTest extends Specification { def is =

  "Writes"                                                                      ^
    "TaskオブジェクトをJson.toJsonでJsValueに変換できること"                    ! e1^
                                                                                p^
  "Reads"                                                                       ^
    "JsObject[id->Int,name->String]をTaskオブジェクトに変換できること"          ! e2^
    "要素が欠けている場合は変換に失敗すること"                                  ! e3^
    "要素の型が違う場合は変換に失敗すること"                                    ! e4^
                                                                                end

  def e1 = {
    val task = Task(1, "testtask")
    val js = toJson(task)

    ((js \ "id").asOpt[Int] must beSome.which(1 ==)) and
    ((js \ "name").asOpt[String] must beSome.which("testtask" ==))
  }

  def e2 = {
    val js = toJson(
      Map(
         "id" -> toJson(1)
        ,"name" -> toJson("testtask")
      )
    )
    val task = js.asOpt[Task]

    task must beSome.which(Task(1, "testtask") ==)
  }

  def e3 = {
    val js = toJson(
      Map(
        "name" -> toJson("testtask")
      )
    )
    val task = js.asOpt[Task]

    task must beNone
  }

  def e4 = {
    val js = toJson(
      Map(
         "id" -> toJson("1")
        ,"name" -> toJson(123)
      )
    )
    val task = js.asOpt[Task]

    task must beNone
  }

}
