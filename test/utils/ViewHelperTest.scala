package org.nisshiee.toban.util

import org.specs2._, matcher.DataTables
import org.nisshiee.toban.model._

import ViewHelper._

object ViewHelperTest extends Specification with DataTables { def is =

  "ViewHelper"                                                                  ^
    "RichMember"                                                                ^
      "labelClass"                                                              ^
         "statusがDeletedの場合はlabel-inverseを返す"                           ! e1^
         "statusがDeletedでない場合はcolorに対応したlabelクラスを返す"          ! e2^
                                                                                p^
      "btnClass"                                                                ^
         "statusがDeletedの場合はbtn-inverseを返す"                             ! e3^
         "statusがDeletedでない場合はcolorに対応したbtnクラスを返す"            ! e4^
                                                                                end

  def e1 = Member(1, "testmember", Member.Deleted, Member.Blue).labelClass must equalTo("label-inverse")

  def e2 =
    "color"                | "label"           |
    Member.Blue            ! "label-info"      |
    Member.Color.Undefined ! "label-info"      |
    Member.Green           ! "label-success"   |
    Member.Yellow          ! "label-warning"   |
    Member.Red             ! "label-important" |> { (color, label) =>
      Member(1, "testmember", Member.Normal, color).labelClass must equalTo(label)
    }

  def e3 = Member(1, "testmember", Member.Deleted, Member.Blue).btnClass must equalTo("btn-inverse")

  def e4 =
    "color"                | "btn"         |
    Member.Blue            ! "btn-info"    |
    Member.Color.Undefined ! "btn-info"    |
    Member.Green           ! "btn-success" |
    Member.Yellow          ! "btn-warning" |
    Member.Red             ! "btn-danger"  |> { (color, btn) =>
      Member(1, "testmember", Member.Normal, color).btnClass must equalTo(btn)
    }
}
