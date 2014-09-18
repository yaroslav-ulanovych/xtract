package xtract.docs.adapters

case class Person(name: String, age: Int)
object John extends Person("John", 42)

import xtract.{DefaultReadParams, FunSuite, read}

// doc begin
// # <a name="adapters-section">Adapters</a>
// One of the goals of this library is to provide an ability to work with different data structures.
// To make read function independent from data it's reading from, a notion of adapters was introduced.
// Adapter is a key value like interface to your data source.
// include Adapter
// So you can parse json, xml, jdbc result sets, virtually everything you can write an adapter for.
// Implementations for map like data structures are extremely simple, as we can see by the
// examples of built in adapters for scala and java maps.
// include MapAdapter
// include JavaMapAdapter
// doc end

class MapReaderTest extends FunSuite {
  test("test") {
// doc begin
// Map adapter is the default one, so you don't need to configure anything to use it.
    val data = Map("name" -> "John", "age" -> 42)
    val person = read[Person] from data
    person shouldBe Person("John", 42)
// doc end
  }
}

// doc begin
// To specify an adapter to use, you have to prepare proper params object
// doc end
class JavaMapReaderTest extends FunSuite {
  test("test") {
//    val data = new java.util.HashMap[String, Any]()
//    data.put("name", "John")
//    data.put("age", 42)
//    implicit val params = DefaultParams + JavaMapAdapter
  }
}


//
// And it's not hard to do so, let's write one for jdbc result set, for example.
import xtract.Reader
// doc end
class AdaptersSection extends FunSuite {
  test("writing custom adapter") {
// doc begin
    import java.sql._

    object ResultSetReader extends Reader[ResultSet] {
      def get(data: ResultSet, key: String): Option[Any] = {
        val meta = data.getMetaData
        val columns = (1 to meta.getColumnCount).map({ i =>
          (meta.getColumnName(i), meta.getColumnType(i))
        }).toMap
        columns.get(key.toUpperCase) match {
// And yeah, you don't need to use SQL_NAMING_CONVENTION in case classes
// or perform field naming conversions in adapters, there is a dedicated
// [field naming conventions](#field-naming-conventions-section) support.
// I use `toUpperCase` here for simplicity, cause example is focused on
// data extracting, not field naming conventions.
          case Some(Types.INTEGER) => Some(data.getInt(key))
          case Some(Types.VARCHAR) => Some(data.getString(key))
          case Some(_) => ??? // and so on
          case None => None
        }
      }
    }
// Let's check it.
    Class.forName("org.h2.Driver")
    val conn = DriverManager.getConnection("jdbc:h2:mem:")
    val stmt = conn.createStatement()
    stmt.execute("create table Person(name varchar, age int)")
    stmt.execute("insert into Person values('John', 42)")
    val resultSet = stmt.executeQuery("select * from Person")
    resultSet.next() shouldBe true
    implicit val params = DefaultReadParams + ResultSetReader
    val person = read[Person] from resultSet
    person shouldBe Person("John", 42)
// doc end
  }
}
