package com.agilogy.play.json.helpers

import play.api.libs.json._
import org.scalactic.TypeCheckedTripleEquals._

object Helpers {

  import Builders._

  trait WritesExtensions[A, W[A] <: Writes[A], WR[A] <: Writes[A]] {

    val originalWrites: W[A]
    val writesBuilder: WritesBuilder[W, WR]

    def writeSettingKey[B: Writes](key: String, value: A => Option[B]): WR[A] =
      writeSettingKeyWhen(key, _ => true, value)

    def writeSettingKeyWhen[B](key: String, conditionFunction: A => Boolean,
      replaceFunction: A => Option[B])(implicit w: Writes[B]): WR[A] = {
      val res: Writes[A] = new Writes[A] {
        override def writes(o: A): JsValue = {
          val originalResult = originalWrites.writes(o)
          originalResult match {
            case jso: JsObject if conditionFunction(o) =>
              (jso - key) ++ JsObject(replaceFunction(o).map(v => key -> w.writes(v)).toSeq)
            case jso: JsObject =>
              originalResult
            case _ =>
              throw new IllegalArgumentException(s"Can't set key on a ${originalResult.getClass.getSimpleName}")
          }
        }
      }
      writesBuilder.buildWrites(originalWrites, res)
    }

    def writeRemovingKeyWhen[B](key: String, condition: A => Boolean): WR[A] = writeSettingKeyWhen[String](key, condition, _ => None)

    private def writeSettingKeyWhenJson(key: String, conditionFunction: JsValue => Boolean,
      replaceFunction: JsValue => Option[JsValue]): WR[A] = {
      val w: Writes[A] = new Writes[A] {
        override def writes(o: A): JsValue = {
          val originalResult = originalWrites.writes(o)
          originalResult match {
            case jso: JsObject if conditionFunction(originalResult) =>
              (jso - key) ++ JsObject(replaceFunction(originalResult).map(v => key -> v).toSeq)
            case jso: JsObject =>
              originalResult
            case _ =>
              throw new IllegalArgumentException(s"Can't set key on a ${originalResult.getClass.getSimpleName}")
          }
        }
      }
      writesBuilder.buildWrites(originalWrites, w)
    }

    def writeWithDefaultValue[B](key: String, defaultValue: B)(implicit ev1: Writes[B]): WR[A] =
      writeSettingKeyWhenJson(key, _ \ key === ev1.writes(defaultValue), _ => None)

  }

  implicit class ImplicitWritesExtensions[A, W[A] <: Writes[A]](val originalWrites: W[A]) extends WritesExtensions[A, W, Writes] {
    override val writesBuilder: WritesBuilder[W, Writes] = new WritesBuilder[W, Writes] {
      override def buildWrites[Elem](from: W[Elem], w: Writes[Elem]): Writes[Elem] = w
    }
  }

  trait ReadsExtensions[A, R[A] <: Reads[A], RR[A] <: Reads[A]] {

    val originalReads: R[A]
    val readsBuilder: ReadsBuilder[R, RR]

    def readWithDefaultValue[B](key: String, defaultValue: B)(implicit ev: Writes[B]): RR[A] = {
      val r: Reads[A] = new Reads[A] {
        override def reads(json: JsValue): JsResult[A] = {
          val res: JsValue = json match {
            case jsObj: JsObject if !jsObj.keys.contains(key) => jsObj + (key -> ev.writes(defaultValue))
            case _ => json
          }
          originalReads.reads(res)
        }
      }
      readsBuilder.buildReads(originalReads, r)
    }

    def readWithAlternativeKey[B](key: String, alternativeKey: String)(implicit f: Format[B]): RR[A] = {
      val r: Reads[A] = new Reads[A] {
        override def reads(json: JsValue): JsResult[A] = {
          val keyValue = json.\(key).asOpt[B](f)
          val alternativeKeyValue = json.\(alternativeKey).asOpt[B](f)

          val res: JsValue = json match {
            case jsObj: JsObject if keyValue.isEmpty && alternativeKeyValue.isDefined =>
              jsObj + (key -> f.writes(alternativeKeyValue.get))
            case _ => json
          }
          originalReads.reads(res)
        }
      }
      readsBuilder.buildReads(originalReads, r)
    }

  }

  implicit class ImplicitReadsExtensions[A](val originalReads: Reads[A]) extends ReadsExtensions[A, Reads, Reads] {
    override val readsBuilder: ReadsBuilder[Reads, Reads] = new ReadsBuilder[Reads, Reads] {
      override def buildReads[Elem](from: Reads[Elem], r: Reads[Elem]): Reads[Elem] = r
    }
  }

  implicit class FormatExtensions[A, F[A] <: Format[A]](originalFormat: F[A])
      extends WritesExtensions[A, F, Format] with ReadsExtensions[A, F, Format] {

    override val originalReads: F[A] = originalFormat
    override val originalWrites: F[A] = originalFormat

    override val readsBuilder: ReadsBuilder[F, Format] = new ReadsBuilder[F, Format] {
      override def buildReads[Elem](from: F[Elem], r: Reads[Elem]): Format[Elem] = Format[Elem](r, from)
    }
    override val writesBuilder: WritesBuilder[F, Format] = new WritesBuilder[F, Format] {
      override def buildWrites[Elem](from: F[Elem], w: Writes[Elem]): Format[Elem] = Format(from, w)
    }

    def withDefaultValue[B](key: String, defaultValue: B)(implicit ev: Writes[B]): Format[A] =
      this.readWithDefaultValue(key, defaultValue).writeWithDefaultValue(key, defaultValue)
  }

}
