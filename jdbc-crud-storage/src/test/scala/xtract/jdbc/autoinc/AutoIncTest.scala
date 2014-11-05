package xtract.jdbc.autoinc

import java.sql.DriverManager
import java.util.UUID

import org.scalatest.{Matchers, FunSuite}
import xtract.jdbc.{JdbcCrudStorage, DbSettings}
import xtract.{Underscore, UpperCase, Obj}

class User extends Obj {
  val id = int.unique.autoInc
}

class AutoIncTest extends FunSuite with Matchers {
  val fnc = UpperCase.delimitedBy(Underscore)

  test("autoinc") {
    val dbSettings = DbSettings("org.h2.Driver", "jdbc:h2:mem:" + UUID.randomUUID(), "", "")
    val conn = DriverManager.getConnection(dbSettings.url)
    val stmt = conn.createStatement()
    stmt.execute("create table user(id int primary key auto_increment, id2 int not null auto_increment);")
    stmt.close()

    val storage = new JdbcCrudStorage(dbSettings, fnc)
    try {
      storage.inTransaction {
        import xtract.query.QueryDsl._
        val user = new User
        storage.insert(user)
        user.id() shouldBe 1
      }
    } finally {
      storage.close
      conn.close
    }

  }
}
