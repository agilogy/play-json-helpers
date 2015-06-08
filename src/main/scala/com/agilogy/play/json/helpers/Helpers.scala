package com.agilogy.play.json.helpers

import play.api.libs.json._

object Helpers {

  import Builders._

  implicit class WritesExtensions[A, W[A] <: Writes[A]](originalWrites: W[A]) {

    def writeSettingKey[B, WR[_] <: Writes[_]](key: String, value: A => Option[B])(implicit ev1: Writes[B], builder: WritesBuilder[W, WR]): WR[A] = writeSettingKeyWhen(key, _ => true, value)

    def writeSettingKeyWhen[B, WR[_] <: Writes[_]](key: String, conditionFunction: A => Boolean, replaceFunction: A => Option[B])(implicit ev1: Writes[B], builder: WritesBuilder[W, WR]): WR[A] = {
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

    private def writeSettingKeyWhenJson[WR[_] <: Writes[_]](key: String, conditionFunction: JsValue => Boolean, replaceFunction: JsValue => Option[JsValue])(implicit builder: WritesBuilder[W, WR]): WR[A] = {
      val w: Writes[A] = new Writes[A] {
        override def writes(o: A): JsObject = {
          val res = originalWrites.writes(o)
          res match {
            case jsObj: JsObject if conditionFunction(res) =>
              (jsObj - key) ++ JsObject(replaceFunction(res).map(v => key -> v).toSeq)
            case jsObj: JsObject => jsObj
            case _ => throw new IllegalArgumentException()
          }
        }
      }
      builder.buildWrites(originalWrites, w)
    }

    def writeWithDefaultValue[B, WR[_] <: Writes[_]](key: String, defaultValue: B)(implicit ev1: Writes[B], builder: WritesBuilder[W, WR]): WR[A] = writeSettingKeyWhenJson(key, _ \ key == ev1.writes(defaultValue), _ => None)

  }

  implicit class ReadsExtensions[A, R[A] <: Reads[A]](originalReads: R[A])(implicit builder: ReadsBuilder[R]) {

    def readWithDefaultValue[B](key: String, defaultValue: B)(implicit ev: Writes[B]): R[A] = {
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

  implicit class FormatExtensions[A, F[A] <: Format[A]](originalFormat: F[A])(implicit readsBuilder: ReadsBuilder[F]) {
    def withDefaultValue[B, FR[_] <: Format[_]](key: String, defaultValue: B)(implicit ev: Writes[B], builder: WritesBuilder[F, FR]): FR[A] =
      originalFormat.readWithDefaultValue(key, defaultValue).writeWithDefaultValue(key, defaultValue)

  }

}
