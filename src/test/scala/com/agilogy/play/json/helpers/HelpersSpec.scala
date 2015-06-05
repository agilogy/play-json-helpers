package com.agilogy.play.json.helpers

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{ FlatSpec, Matchers, OptionValues }
import play.api.libs.json._

class HelpersSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals with OptionValues {

  import Helpers._
  import play.api.libs.json.Json

  case class Person(name: String, age: Int, company: Option[Company])
  case class Company(name: String, cif: Long)

  "Json helper read with default value" should "return a Reads when using with Reads" in {
    implicit val companyFmt: Format[Company] = Json.format[Company]
    Json.reads[Person].readWithDefaultKey("company", Company("Agilogy", 123456)).isInstanceOf[Reads[Person]] shouldBe true
  }

  "Json helper read with default value" should "return a Format when using with Format" in {
    implicit val companyFmt: Format[Company] = Json.format[Company]
    Json.format[Person].readWithDefaultKey("company", Company("Agilogy", 123456)).isInstanceOf[Format[Person]] shouldBe true
  }

  "Json helper read with default value" should "let the value of the json if the key exists" in {
    implicit val companyFmt: Format[Company] = Json.format[Company]
    implicit val personReads: Reads[Person] = Json.reads[Person]
      .readWithDefaultKey("company", Company("Agilogy", 123456))

    val person: Person = personReads.reads(Json.parse(
      """
      {
        "name": "John",
        "age": 30,
        "company": {
          "name" : "World Inc.",
          "cif" : 1010101
        }
      }
      """
    )).get

    person should ===(Person("John", 30, Some(Company("World Inc.", 1010101))))
  }

  "Json helper read with default value" should "add the default value to the json if the key doesn't exist" in {
    implicit val companyFmt: Format[Company] = Json.format[Company]
    implicit val personReads: Reads[Person] = Json.reads[Person]
      .readWithDefaultKey("company", Company("Agilogy", 123456))

    val person: Person = personReads.reads(Json.parse(
      """
      {
        "name": "John",
        "age": 30
      }
      """
    )).get

    person should ===(Person("John", 30, Some(Company("Agilogy", 123456))))
  }
}
