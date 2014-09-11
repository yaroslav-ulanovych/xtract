package xtract.docs.converters

case class Person(id: Int, name: String)

import xtract.{Reader, BadFieldValueException, DefaultReadParams, FunSuite, read}


// doc begin
// # <a name="converters-section">Converters</a>
// There are cases when a data source contains a value of type that is
// slightly different from what you expect in your case class.
// For instance json ast from json4s uses big ints,
// while in scala we usually use ints. Obviously you'll get a [bad field
// value exception](#bad-field-value-exception-section), when you try to read such data source.
import org.json4s.JObject
import org.json4s.native.JsonParser
// doc end
class ConvertersSection extends FunSuite {
// doc begin
  val json = """{"id": 2, "name": "John"}"""
  val jval = JsonParser.parse(json)
  val jobj = jval.asInstanceOf[JObject]
// doc end
  test("json read fail") {
// doc begin
    val e = intercept[BadFieldValueException] {
      read[Person] from jobj.values
    }
    e.valueType shouldBe classOf[BigInt]
// doc end
  }

  test("custom converer") {
// doc begin
// We could fix that problem writing a proper adapter.
    object JsonReader extends Reader[JObject] {
      def get(data: JObject, key: String): Option[Any] = {
        data.values.get(key) map {
          case x: BigInt => x.toInt
          case x => x
        }
      }
    }
    read[Person].from(jobj)(DefaultReadParams + JsonReader) shouldBe Person(2, "John")
// But that burdens adapter, whose task is
// data extracting, while data converting can be a separate step. That's more
// composable, since you can use one converter with different adapters and one
// adapter with different converters.
// Converter is just a function from one type to an option of another type
// include Converter
// In fact it's a little bite more than just a function, cause it should carry
// types, so that read function is able to find a proper converter,
// but that is done implicitly and hidden from user.
// Let's create one.
    import xtract.Converter
    val BigIntToInt = Converter((x: BigInt) => if (x.isValidInt) Some(x.toInt) else None)
// and check that it works
    val person = read[Person].from(jobj.values)(DefaultReadParams + BigIntToInt)
    person shouldBe Person(2, "John")
// doc end
  }
}


// doc begin
// Another possible case for converters is parsing enumerations,
// which are often kept as strings or integers in data source, but
// we want a normal enumeration in case class.
// include JavaEnumConverter
import java.lang.Thread.State
case class Thread(state: State)
// doc end

class Part2 extends FunSuite {
  test("enum converter") {
    import xtract.JavaEnumConverter
// doc begin
    implicit val params = DefaultReadParams + JavaEnumConverter
//
    read[Thread] from Map("state" -> 1) shouldBe {
      Thread(State.RUNNABLE)
    }

    read[Thread] from Map("state" -> "WAITING") shouldBe {
      Thread(State.WAITING)
    }
    
    intercept[BadFieldValueException] {
      read[Thread] from Map("state" -> "not an enum")
    }
  }
}