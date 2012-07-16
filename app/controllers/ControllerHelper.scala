package org.nisshiee.toban.controller

import play.api.mvc._
import scalaz._, Scalaz._
import org.nisshiee.toban.model._

trait ControllerHelper {

  val memberNameKey = "member_name"
  val memberIdKey = "member_id"
  val taskNameKey = "task_name"
  val taskIdKey = "task_id"
  val dateKey = "date"

  def paramOpt[T](key: String)(implicit req: Request[Map[String, Seq[String]]], ext: Extractor[T]): Option[T] =
    req.body.get(key) >>= ext.extract

  def paramVld[E, T](key: String)(err: => E)(implicit req: Request[Map[String, Seq[String]]], ext: Extractor[T]): Validation[E, T] =
    paramOpt[T](key).toSuccess(err)

  trait Extractor[T] {

    def extract(data: Seq[String]): Option[T]
  }

  def extractor[T](f: Seq[String] => Option[T]): Extractor[T] = new Extractor[T] {

    def extract(data: Seq[String]) = f(data)
  }

  def extractorBy[T, B](f: B => Option[T])(implicit extB: Extractor[B]): Extractor[T] =
    extractor[T] { extB.extract(_) >>= f }

  implicit lazy val StringExtractor = extractor {
    case Seq(s) => s.some
    case _ => none
  }

  implicit lazy val IntExtractor = extractorBy[Int, String] { _.parseInt.toOption }

  implicit lazy val DateExtractor = extractorBy[LocalDate, String] { str2DateOpt }
}
