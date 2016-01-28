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

  behavior of "Read with alternative key"

  val jsonPersonWithOldFormat = Json.parse(
    """
      {
        "oldNameKey": "John",
        "oldAgeKey": 30
      }
    """
  )

  val jsonPersonWithOldFormatWithCompany = Json.parse(
    """
      {
        "oldNameKey": "John",
        "age": 30,
        "company": {
          "name" : "World Inc."
        }
      }
    """
  )

  val jsonPersonWithCompanyInOldFormat = Json.parse(
    """
      {
        "name": "John",
        "age": 30,
        "oldCompanyKey": {
          "name" : "World Inc.",
          "cif" : 1010101
        }
      }
    """
  )

  it should "read json with old format" in {
    implicit val companyReads = companyFmt
    implicit val personReads: Reads[Person] = Json.reads[Person]
      .readWithAlternativeKey[String]("name", "oldNameKey")
      .readWithAlternativeKey[Int]("age", "oldAgeKey")
    val person: Person = personReads.reads(jsonPersonWithOldFormat).get
    assert(person === personWithoutCompany)
  }

  it should "combine correctly read with alternative key and read with default value" in {
    implicit val companyWrites = companyFmt
    implicit val personReads: Reads[Person] = Json.reads[Person]
      .readWithAlternativeKey[String]("name", "oldNameKey")
      .readWithAlternativeKey[Int]("age", "oldAgeKey")
      .readWithDefaultValue("company", agilogy)
    val res = personReads.reads(jsonPersonWithOldFormat)
    val person: Person = personReads.reads(jsonPersonWithOldFormat).get
    assert(person === Person("John", 30, Some(agilogy)))
  }

  it should "be compatible with extended readers in nested classes" in {
    implicit val companyReader = companyFmt.readWithDefaultValue("cif", 1010101)
    implicit val personReads: Reads[Person] = Json.reads[Person]
      .readWithAlternativeKey[String]("name", "oldNameKey")
      .readWithDefaultValue("age", 30)
    val person: Person = personReads.reads(jsonPersonWithOldFormatWithCompany).get
    assert(person === Person("John", 30, Some(Company("World Inc.", 1010101))))
  }

  it should "read correctly an alternative optional key" in {
    implicit val companyReader = companyFmt
    implicit val personReads: Reads[Person] = Json.reads[Person]
      .readWithAlternativeKey[Company]("company", "oldCompanyKey")
    val person: Person = personReads.reads(jsonPersonWithCompanyInOldFormat).get
    assert(person === Person("John", 30, Some(Company("World Inc.", 1010101))))

    val readPersonWithoutCompany: Person = personReads.reads(jsonPersonWithoutCompany).get
    assert(readPersonWithoutCompany === personWithoutCompany)

  }

  behavior of "Writes helper"

  //  they should "produce an exception when setting a key on no JsObject" in {
  //    val exception = intercept[IllegalArgumentException] {
  //      Writes.IntWrites.writeSettingKeyWhen("age", _ === 0, _ => Some(18)).writes(18)
  //    }
  //  }

  they should "overwrites a property in an object" in {
    implicit val companyFormat = companyFmt
    val w = Json.writes[Person].writeSettingKey("age", _ => Some(18))
    val p = personWithoutCompany
    val res = w.writes(p)
    assert((res \ "name").get === JsString("John"))
    assert((res \ "age").get === JsNumber(18))
  }

  they should "overwrites a property in an object only if a condition is met" in {
    implicit val companyFormat = companyFmt
    val w = Json.writes[Person].writeSettingKeyWhen("age", _.name == "Jordi", _ => Some(18))
    val p = personWithoutCompany
    val res = w.writes(p)
    assert(res === jsonPersonWithoutCompany)
    val j = Person("Jordi", 38, Some(agilogy))
    val res2 = w.writes(j)
    assert((res2 \ "name").get === JsString("Jordi"))
    assert((res2 \ "age").get === JsNumber(18))
    assert((res2 \ "company").get === companyFormat.writes(agilogy))
  }

  they should "overwrites a property in an object (when applied on a format) only if a condition is met" in {
    implicit val companyFormat = companyFmt
    // IntelliJ needs some hints to understand the line. Otherwise, the following line is totally valid:
    //    val f: Format[Person] = Json.format[Person].writeSettingKeyWhen("age", _.name == "Jordi", _ => Some(18))
    val f: Format[Person] = Json.format[Person].writeSettingKeyWhen("age", _.name == "Jordi", _ => Some(18))
    val p = personWithoutCompany
    val res = f.writes(p)
    assert(res === jsonPersonWithoutCompany)
    val j = Person("Jordi", 38, Some(agilogy))
    val res2 = f.writes(j)
    assert((res2 \ "name").get === JsString("Jordi"))
    assert((res2 \ "age").get === JsNumber(18))
    assert((res2 \ "company").get === companyFormat.writes(agilogy))
    assert(f.reads(jsonPersonWithoutCompany).get === p, "The reads continues working normally")
  }

  it should "remove key upon a certain condition" in {
    implicit val companyFormat = companyFmt
    val f = Json.format[Person].writeRemovingKeyWhen("age", _.name == "Jordi")
    val j = Person("Jordi", 38, Some(agilogy))
    val res = f.writes(j)
    assert((res \ "name").get === JsString("Jordi"))
    assert((res \ "age").toOption.isEmpty)
  }

  behavior of "Format helper withDefaultValue"

  //  they should "produce an exception when setting a key on no JsObject" in {
  //    val exception = intercept[IllegalArgumentException] {
  //      Format(Reads.IntReads, Writes.IntWrites).withDefaultValue("age", 18).writes(18)
  //    }
  //
  //  }

  it should "ommit a default value when writting and read it when not present" in {
    implicit val companyFormat = companyFmt
    val f: Format[Person] = Json.format[Person].withDefaultValue("company", agilogy).withDefaultValue("name", "John")
    val j = Person("Jordi", 38, Some(agilogy))
    val json = f.writes(j)
    assert((json \ "company").asOpt === None)

    assert(f.reads(jsonPersonWithoutCompany).get === personWithoutCompany.copy(company = Some(agilogy)))

    assert(f.reads(json).get === j)
  }

  case class Optionals(foo: Option[String], bar: Option[Int])

  it should "chain default values" in {
    val f: Format[Optionals] = Json.format[Optionals].withDefaultValue("foo", "fooValue").withDefaultValue("bar", 43)
    val v = Optionals(Some("fooValue"), Some(43))
    val json = f.writes(v)
    assert(json === Json.obj())
    val opts = f.reads(Json.obj())
    assert(opts.get === v)
  }

  it should "chain multiple transformation" in {
    val f = Json.format[Optionals]
      .writeWithDefaultValue("eanCode", Seq.empty[String])
      .writeWithDefaultValue("loyaltyPoints", Some(BigDecimal(0)))
      .writeSettingKey("offlineLoyaltyPoints", _.foo.filter(_ != ""))
    //TODO: Check that the composed format works as expected
    assert(f.isInstanceOf[Format[Optionals]])

  }

  //  it should "chain transformations" in {
  //    implicit val companyFormat = companyFmt
  //    val f: Format[Person] = Json.format[Person].readWithDefaultValue("name", "John").readWithDefaultValue("age", 33).withDefaultValue("company", agilogy).writeSettingKey("hasCompany", p => Some(p.company.nonEmpty))
  //  }

}
