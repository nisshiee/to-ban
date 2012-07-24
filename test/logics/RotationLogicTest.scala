package org.nisshiee.toban.logic

import org.specs2._

import org.nisshiee.toban.model._

class RotationLogictest extends Specification { def is =

  "delete"                                                                      ^
    "指定したIDが無い場合はNoId"                                                ! e1^
    "指定したIDがある場合は削除"                                                ! e2^
                                                                                p^
  "insert"                                                                      ^
    "MinScore <-> MaxScore間に空きがない場合はFull"                             ! e3^
    "挿入位置が負の場合はIllegalPos"                                            ! e4^
    "挿入位置が<length>より大きい場合はIllegalPos"                              ! e5^
    "挿入位置のscoreに隙間があれば、中央に挿入"                                 ! e6^
    "挿入位置のscoreに隙間がない場合、挿入後全体を均質化"                       ! e7^
                                                                                p^
  "move"                                                                        ^
    "指定したIDがない場合はNoId"                                                ! e8^
    "挿入位置が負の場合はIllegalPos"                                            ! e9^
    "挿入位置が<length - 1>より大きい場合はIllegalPos"                          ! e10^
    "挿入位置のscoreに隙間があれば、中央に移動"                                 ! e11^
    "挿入位置のscoreに隙間がない場合、移動後全体を均質化"                       ! e12^
                                                                                end

  def e1 = ko
  def e2 = ko
  def e3 = ko
  def e4 = ko
  def e5 = ko
  def e6 = ko
  def e7 = ko
  def e8 = ko
  def e9 = ko
  def e10 = ko
  def e11 = ko
  def e12 = ko
}
