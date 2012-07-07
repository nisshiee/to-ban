package org.nisshiee.toban.model

import org.specs2._
import org.specs2.matcher.DataTables

import play.api.libs.json.Json._

class LocalDateJsonTest extends Specification with DataTables { def is =

  "Writes"                                                                      ^
    "LocalDateオブジェクトをJson.toJsonでJsValueに変換できること"               ! e1^
                                                                                p^
  "Reads"                                                                       ^
    "JsStringをTaskオブジェクトに変換できること"                                ! e2^
    "型が違う場合は変換に失敗すること"                                          ! e3^
    "文字列の形式が不正な場合は変換に失敗すること"                              ! e4^
                                                                                end

  def e1 = {
    val date = LocalDate.today
    val js = toJson(date)
    js.asOpt[String] must beSome.which(date.toString ==)
  }

  def e2 =
    "dateStr"    || "date"                      |
    "2012-01-01" !! new LocalDate("2012-01-01") |
    "2012-02-29" !! new LocalDate("2012-02-29") |
    "2100-12-31" !! new LocalDate("2100-12-31") |> { (dateStr, date) =>
      val js = toJson(dateStr)
      js.asOpt[LocalDate] must beSome.which(date ==)
    }

  def e3 = {
    val intJs = toJson(1)
    val aryJs = toJson(List("1", "2"))
    (intJs.asOpt[LocalDate] must beNone) and
    (aryJs.asOpt[LocalDate] must beNone)
  }

  def e4 =
    "dateStr"    || "result" |
    "2012/01/01" !! None     |
    "日本語"     !! None     |
    "21001231"   !! None     |> { (dateStr, result) =>
      val js = toJson(dateStr)
      js.asOpt[LocalDate] must equalTo(result)
    }
}
