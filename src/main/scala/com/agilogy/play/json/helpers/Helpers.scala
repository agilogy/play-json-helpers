package com.agilogy.play.json.helpers

import play.api.libs.json._

object Helpers {

  import Builders._

  implicit class WritesExtensions[A, -W[A] <: Writes[A], WR[_] <: Writes[A]](oWrites: W[A])(implicit builder: WritesBuilder[W, WR]) {

    def writeWithOverridedKeyWhen[B](key: String, conditionFunction: A => Boolean, replaceFunction: A => Option[B])(implicit ev1: Writes[B]): WR[A] = {
      val w: Writes[A] = new Writes[A] {
        override def writes(o: A): JsValue = {
          val res = oWrites.writes(o)
          res match {
            case jsObj: JsObject if conditionFunction(o) =>
              (jsObj - key) ++ JsObject(replaceFunction(o).map(v => key -> ev1.writes(v)).toSeq)
            case _ => res
          }
        }
      }
      builder.buildWrites(oWrites, w)
    }
  }

  implicit class ReadsExtensions[A, R[A] <: Reads[A]](oReads: R[A])(implicit builder: ReadsBuilder[R]) {
    def readWithDefaultKey[B](key: String, defaultValue: B)(implicit ev: Writes[B]): R[A] = {
      val r: Reads[A] = new Reads[A] {
        override def reads(json: JsValue): JsResult[A] = {
          val res: JsValue = json match {
            case jsObj: JsObject if !jsObj.keys.contains(key) => jsObj + (key -> ev.writes(defaultValue))
            case _ => json
          }
          oReads.reads(res)
        }
      }
      builder.buildReads(oReads, r)
    }

  }
}
