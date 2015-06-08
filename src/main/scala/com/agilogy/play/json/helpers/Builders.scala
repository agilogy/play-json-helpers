package com.agilogy.play.json.helpers

import play.api.libs.json._

object Builders {

  trait WritesBuilder[-W[_] <: Writes[_], +To[_] <: Writes[_]] {
    def buildWrites[Elem](from: W[Elem], w: Writes[Elem]): To[Elem]
  }

  object WritesBuilder {

    implicit val writesInstance: WritesBuilder[Writes, Writes] = new WritesBuilder[Writes, Writes] {
      override def buildWrites[Elem](from: Writes[Elem], w: Writes[Elem]): Writes[Elem] = w
    }

    implicit val formatInstance = new WritesBuilder[Format, Format] {
      override def buildWrites[Elem](from: Format[Elem], w: Writes[Elem]): Format[Elem] = Format(from, w)
    }
  }

  trait ReadsBuilder[R[_] <: Reads[_]] {
    def buildReads[Elem](from: R[Elem], r: Reads[Elem]): R[Elem]
  }

  object ReadsBuilder {

    implicit val readsInstance: ReadsBuilder[Reads] = new ReadsBuilder[Reads] {
      override def buildReads[Elem](from: Reads[Elem], r: Reads[Elem]): Reads[Elem] = r
    }
    implicit val formatInstance: ReadsBuilder[Format] = new ReadsBuilder[Format] {
      override def buildReads[Elem](from: Format[Elem], r: Reads[Elem]): Format[Elem] = Format(r, from)
    }
    implicit val oFormatInstance: ReadsBuilder[OFormat] = new ReadsBuilder[OFormat] {
      override def buildReads[Elem](from: OFormat[Elem], r: Reads[Elem]): OFormat[Elem] = OFormat(r, from)
    }
  }

}
