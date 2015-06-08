package com.agilogy.play.json.helpers

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{ FlatSpec, Matchers, OptionValues }
import play.api.libs.json._

class HelpersSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals with OptionValues {

  import Helpers._
  import play.api.libs.json.Json

  case class Person(name: String, age: Int, company: Option[Company])
  case class Company(name: String, cif: Long)
  val companyFmt: Format[Company] = Json.format[Company]
  val agilogy = Company("Agilogy", 123456)

  behavior of "Json helper read with default value"

  it should "return a Reads when using with Reads" in {
    implicit val companyFormat = companyFmt
    Json.reads[Person].readWithDefaultValue("company", Company("Agilogy", 123456)).isInstanceOf[Reads[Person]] shouldBe true
  }

  it should "return a Format when using with Format" in {
    implicit val companyFormat = companyFmt
    Json.format[Person].readWithDefaultValue("company", Company("Agilogy", 123456)).isInstanceOf[Format[Person]] shouldBe true
  }

  val jsonPersonWithCompany: JsValue = Json.parse(
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
  )

  val personWithoutCompany = Person("John", 30, company = None)

  val jsonPersonWithoutCompany = Json.parse(
    """
      {
        "name": "John",
        "age": 30
      }
    """
  )

  it should "let the value of the json if the key exists" in {
    implicit val companyWrites = companyFmt
    implicit val personReads: Reads[Person] = Json.reads[Person].readWithDefaultValue("company", agilogy)
    val person: Person = personReads.reads(jsonPersonWithCompany).get
    assert(person === Person("John", 30, Some(Company("World Inc.", 1010101))))
  }

  it should "add the default value to the json if the key doesn't exist" in {
    implicit val companyWrites = companyFmt
    implicit val personReads: Reads[Person] = Json.reads[Person].readWithDefaultValue("company", agilogy)
    val person: Person = personReads.reads(jsonPersonWithoutCompany).get
    assert(person === Person("John", 30, Some(agilogy)))
  }

  it should "transform a format as well, returning a format" in {
    implicit val companyWrites = companyFmt
    implicit val f: Format[Person] = Json.format[Person].readWithDefaultValue("company", agilogy)
    assert(f.reads(jsonPersonWithoutCompany).get == Person("John", 30, company = Some(agilogy)))
  }

  behavior of "Writes helper"

  they should "overwrites a property in an object only if a condition is met" in {
    implicit val companyFormat = companyFmt
    val w = Json.writes[Person].writeSettingKeyWhen("age", _.name == "Jordi", _ => Some(18))
    val p = personWithoutCompany
    val res = w.writes(p)
    assert(res === jsonPersonWithoutCompany)
    val j = Person("Jordi", 38, Some(agilogy))
    val res2 = w.writes(j)
    assert(res2 \ "name" === JsString("Jordi"))
    assert(res2 \ "age" === JsNumber(18))
    assert(res2 \ "company" === companyFormat.writes(agilogy))

  }

  they should "overwrites a property in an object (when applied on a format) only if a condition is met" in {
    implicit val companyFormat = companyFmt
    // IntelliJ needs some hints to understand the line. Otherwise, the following line is totally valid:
    //    val f: Format[Person] = Json.format[Person].writeWithOverridedKeyWhen("age", _.name == "Jordi", _ => Some(18))
    val f: Format[Person] = Json.format[Person].writeSettingKeyWhen[Int, Format]("age", _.name == "Jordi", _ => Some(18))
    val p = personWithoutCompany
    val res = f.writes(p)
    assert(res === jsonPersonWithoutCompany)
    val j = Person("Jordi", 38, Some(agilogy))
    val res2 = f.writes(j)
    assert(res2 \ "name" === JsString("Jordi"))
    assert(res2 \ "age" === JsNumber(18))
    assert(res2 \ "company" === companyFormat.writes(agilogy))
    assert(f.reads(jsonPersonWithoutCompany).get === p, "The reads continues working normally")
  }

  behavior of "Format helper withDefaultValue"

  it should "ommit a default value when writting and read it when not present" in {
    implicit val companyFormat = companyFmt
    val f: Format[Person] = Json.format[Person].withDefaultValue("company", agilogy)
    val j = Person("Jordi", 38, Some(agilogy))
    val json = f.writes(j)
    assert((json \ "company").asOpt === None)

    assert(f.reads(jsonPersonWithoutCompany).get === personWithoutCompany.copy(company = Some(agilogy)))

    assert(f.reads(json).get === j)
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
