package xtract.jdbc

import java.sql.DriverManager

import org.scalatest.{Matchers, FunSuite}
import xtract.{AbstractObj, Obj}

trait Account extends AbstractObj

class TwitterAccount extends Account {
  val id = int
}

class GoogleAccount extends Account {
  val id = int
}

class JdbcCrudStorageTest extends FunSuite with Matchers {
  test("bla") {
    val conn = DriverManager.getConnection("jdbc:h2:mem:db1")
    val stmt = conn.createStatement()
    stmt.execute("create table account(type varchar, twitter__id varchar, google__id varchar);")
    val storage = new JdbcCrudStorage(DbSettings("org.h2.Driver", "jdbc:h2:mem:db1", "", ""))
    storage.create(Obj.create[TwitterAccount](_.id := 1))
  }
}
