package org.nisshiee.toban

import org.scala_tools.time.{ Imports => Times }

package object model
  extends Members
     with Tasks
     with Times
     with LocalDates
     with Tobans
     with Memos
{

}
