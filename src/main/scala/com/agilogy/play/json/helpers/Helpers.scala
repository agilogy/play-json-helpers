package com.agilogy.play.json.helpers

import play.api.libs.json._

object Helpers {

  import Builders._

  implicit class WritesExtensions[A, W[A] <: Writes[A]](originalWrites: W[A]) {

    def writeWithOverridedKeyWhen[B, WR[_] <: Writes[_]](key: String, conditionFunction: A => Boolean, replaceFunction: A => Option[B])(implicit ev1: Writes[B], builder: WritesBuilder[W, WR]): WR[A] = {
      val w: Writes[A] = new Writes[A] {
        override def writes(o: A): JsObject = {
          val res = originalWrites.writes(o)
          res match {
            case jsObj: JsObject if conditionFunction(o) =>
              (jsObj - key) ++ JsObject(replaceFunction(o).map(v => key -> ev1.writes(v)).toSeq)
            case jsObj: JsObject => jsObj
            case _ => throw new IllegalArgumentException()
          }
        }
      }
      builder.buildWrites(originalWrites, w)
    }
  }

  implicit class ReadsExtensions[A, R[A] <: Reads[A]](originalReads: R[A])(implicit builder: ReadsBuilder[R]) {
    def readWithDefaultKey[B](key: String, defaultValue: B)(implicit ev: Writes[B]): R[A] = {
      val r: Reads[A] = new Reads[A] {
        override def reads(json: JsValue): JsResult[A] = {
          val res: JsValue = json match {
            case jsObj: JsObject if !jsObj.keys.contains(key) => jsObj + (key -> ev.writes(defaultValue))
            case _ => json
          }
          originalReads.reads(res)
        }
      }
      builder.buildReads(originalReads, r)
    }

  }
}
