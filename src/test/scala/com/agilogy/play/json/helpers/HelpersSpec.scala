package com.agilogy.play.json.helpers

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{ FlatSpec, Matchers, OptionValues }
import play.api.libs.json._

class HelpersSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals with OptionValues {

  import Helpers._
  import play.api.libs.json.Json

  case class Person(name: String, age: Int, company: Option[Company])
  case class Company(name: String, cif: Long)
  implicit val companyFmt: Format[Company] = Json.format[Company]

  "Json helper read with default value" should "return a Reads when using with Reads" in {
    Json.reads[Person].readWithDefaultKey("company", Company("Agilogy", 123456)).isInstanceOf[Reads[Person]] shouldBe true
  }

  "Json helper read with default value" should "return a Format when using with Format" in {
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

  behavior of "Writes helper"

  they should "overwrites a property in an object only if a condition is met" in {
    import Builders._
    // IntelliJ needs some hints to understand the line:
    val ow = Json.writes[Person]
    val w = ow.writeWithOverridedKeyWhen("age", _.name == "Jordi", _ => Some(18))(implicitly[Writes[Int]], WritesBuilder.writesInstance)
    val reads = Json.reads[Person]
    val p = Person("John", 30, company = None)
    val res = w.writes(p)
    assert(reads.reads(res).get === p)
    val j = Person("Jordi", 38, Some(Company("agilogy", 1234567)))
    assert(reads.reads(w.writes(j)).get == j.copy(age = 18))
  }

  they should "overwrites a property in an object (whena applied on a format) only if a condition is met" in {
    import Builders._
    // IntelliJ needs some hints to understand the line:
    //    val f: Format[Person] = Json.format[Person].writeWithOverridedKeyWhen[Int,Format]("age", _.name == "Jordi", _ => Some(18))
    val f: Format[Person] = Json.format[Person].writeWithOverridedKeyWhen("age", _.name == "Jordi", _ => Some(18))
    val p = Person("John", 30, company = None)
    val res = f.writes(p)
    assert(f.reads(res).get === p)
    val j = Person("Jordi", 38, Some(Company("agilogy", 1234567)))
    assert(f.reads(f.writes(j)).get == j.copy(age = 18))
  }

  //  they should "return a writes when invoked on a writes" in {
  //    import Writes.traversableWrites
  //    implicit val pw: Writes[Person] = Json.writes[Person]
  //    val w = new Writes[Seq[Person]] {
  //      override def writes(o: Seq[Person]): JsValue = Json.arr(o.map(p => Json.toJson(p)))
  //    }
  //
  //    val w2 = w.writeWithOverridedKeyWhen("age", _ => true, _ => Some(18))
  //  }
}
