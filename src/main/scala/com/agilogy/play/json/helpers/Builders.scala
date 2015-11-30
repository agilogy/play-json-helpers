package com.agilogy.play.json.helpers

import play.api.libs.json._

object Builders {

  trait WritesBuilder[-W[_] <: Writes[_], +To[_] <: Writes[_]] {
    def buildWrites[Elem](from: W[Elem], w: OWrites[Elem]): To[Elem]
  }

  trait ReadsBuilder[R[_] <: Reads[_], RR[_] <: Reads[_]] {
    def buildReads[Elem](from: R[Elem], r: Reads[Elem]): RR[Elem]
  }

}
