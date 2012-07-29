package org.nisshiee.toban.test

import play.api.test._
import play.api.test.Helpers._
import play.api.Play.current
import play.api.db._

import anorm._

trait TestHelper {

  lazy val delToban = SQL("""TRUNCATE TABLE toban""")
  lazy val delTask = SQL("""TRUNCATE TABLE task""")
  lazy val delMember = SQL("""TRUNCATE TABLE member""")

  def runningEmptyApplication[A](f: => A) = running(FakeApplication()) {

    DB.withTransaction { implicit c =>
      delToban.execute()
      delTask.execute()
      delMember.execute()
    }

    f
  }
}
