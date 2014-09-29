package xtract.jdbc

import java.sql.DriverManager
import java.util.UUID

import org.scalatest.{GivenWhenThen, Matchers, FunSuite}
import xtract.query.{SimpleFieldInClause, Query}
import xtract._

trait Account extends AbstractObj

class TwitterAccount extends Account {
  val id = string
}

class GoogleAccount extends Account {
  val id = string
}

class User extends Obj {
  val id = int.unique
  val name = string
}

class Session extends Obj {
  val id = string.unique
  val userId = int
}

class JdbcCrudStorageTest extends FunSuite with Matchers with GivenWhenThen {
  val fnc = UpperCase.delimitedBy(Underscore)

  test("select all") {
    val dbSettings = DbSettings("org.h2.Driver", "jdbc:h2:mem:" + UUID.randomUUID(), "", "")
    val conn = DriverManager.getConnection(dbSettings.url)
    val stmt = conn.createStatement()
    stmt.execute("create table user(id int primary key, name varchar);")
    stmt.execute("insert into user values (1, 'John');")
    stmt.execute("insert into user values (2, 'Mary');")

    val storage = new JdbcCrudStorage(dbSettings, fnc)
    try {
      storage.inTransaction {
        import xtract.query.QueryDsl._

        val users = storage.select(from[User])

        users.length shouldBe 2

        users(0).id() shouldBe 1
        users(0).name() shouldBe "John"

        users(1).id() shouldBe 2
        users(1).name() shouldBe "Mary"
      }
    } finally {
      storage.close
      conn.close
    }
  }

  test("select unique and not single field") {
    val dbSettings = DbSettings("org.h2.Driver", "jdbc:h2:mem:" + UUID.randomUUID(), "", "")
    val conn = DriverManager.getConnection(dbSettings.url)
    val stmt = conn.createStatement()
    stmt.execute("create table user(id int primary key, name varchar);")
    stmt.execute("insert into user values (1, 'John');")
    stmt.execute("insert into user values (2, 'John');")
    stmt.execute("insert into user values (3, 'Mary');")

    val storage = new JdbcCrudStorage(dbSettings, fnc)

    try {
      storage.inTransaction {
        import xtract.query.QueryDsl._

        val Some(user) = storage.select(from[User].where(_.id eqs 1))

        user.id() shouldBe 1
        user.name() shouldBe "John"

        storage.select(from[User].where(_.id eqs 42)) shouldBe None

        val users = storage.select(from[User].where(_.name eqs "John"))

        users.length shouldBe 2

        users(0).id() shouldBe 1
        users(0).name() shouldBe "John"

        users(1).id() shouldBe 2
        users(1).name() shouldBe "John"

        storage.select(from[User].where(_.name eqs "non existing name")).length shouldBe 0
      }
    } finally {
      storage.close
      conn.close
    }
  }

  test("select in") {
    val dbSettings = DbSettings("org.h2.Driver", "jdbc:h2:mem:" + UUID.randomUUID(), "", "")
    val conn = DriverManager.getConnection(dbSettings.url)
    val stmt = conn.createStatement()
    stmt.execute("create table user(id int primary key, name varchar);")
    stmt.execute("insert into user values (1, 'John');")
    stmt.execute("insert into user values (2, 'Sam');")
    stmt.execute("insert into user values (3, 'Mary');")

    val storage = new JdbcCrudStorage(dbSettings, fnc)

    try {
      storage.inTransaction {
        import xtract.query.QueryDsl._

        val users = storage.select(from[User].where(_.id in List(1, 3)))

        users.length shouldBe 2

        users(0).id() shouldBe 1
        users(0).name() shouldBe "John"

        users(1).id() shouldBe 3
        users(1).name() shouldBe "Mary"
      }
    } finally {
      storage.close
      conn.close
    }
  }

  test("insert multiple rows") {
    val dbSettings = DbSettings("org.h2.Driver", "jdbc:h2:mem:" + UUID.randomUUID(), "", "")
    val conn = DriverManager.getConnection(dbSettings.url)
    val stmt = conn.createStatement()
    stmt.execute("create table user(id int primary key, name varchar);")

    val storage = new JdbcCrudStorage(dbSettings, fnc)

    storage.inTransaction {
      import xtract.query.QueryDsl._

      val user1 = new User

      user1.id := 1
      user1.name := "John"

      val user2 = new User

      user2.id := 2
      user2.name := "Mary"

      storage.insert(List(user1, user2))
    }

    storage.close

    val rs = stmt.executeQuery("select * from user")

    rs.next() shouldBe true

    rs.getInt(1) shouldBe 1
    rs.getString(2) shouldBe "John"

    rs.next() shouldBe true

    rs.getInt(1) shouldBe 2
    rs.getString(2) shouldBe "Mary"

    rs.next() shouldBe false

    rs.close()
    stmt.close()
    conn.close()
  }

  test("bla") {
    val dbSettings = DbSettings("org.h2.Driver", "jdbc:h2:mem:" + UUID.randomUUID(), "", "")
    val conn = DriverManager.getConnection(dbSettings.url)
    val stmt = conn.createStatement()
    stmt.execute("create table user(id int primary key, name varchar);")
    stmt.execute("create table session(id varchar primary key, user_id int references user(id));")

    val storage = new JdbcCrudStorage(dbSettings, fnc)
    try {
      storage.inTransaction {
        import xtract.query.QueryDsl._

        val user = new User

        user.id := 1
        user.name := "John"

        storage.create(user)

        val session = new Session

        session.id := UUID.randomUUID().toString
        session.userId := user.id()

        storage.create(session)

        storage.select(from[Session].where(_.id eqs session.id())).isDefined shouldBe true
      }
    } finally {
      storage.close
      conn.close()
    }
  }

  test("schema support") {
    val dbSettings = DbSettings("org.h2.Driver", "jdbc:h2:mem:" + UUID.randomUUID(), "", "")
    val conn = DriverManager.getConnection(dbSettings.url)
    val stmt = conn.createStatement()
    stmt.execute("create schema aaa;")
    stmt.execute("set schema aaa;")
    stmt.execute("create table user(id int primary key, name varchar);")
    stmt.execute("insert into user values(1, 'John');")

    val storage = new JdbcCrudStorage(dbSettings, fnc, Some("AAA"))
    try {
      storage.inTransaction {
        import xtract.query.QueryDsl._
        storage.select(from[User]).size shouldBe 1
      }
    } finally {
      storage.close
      conn.close()
    }
  }

  test("formatTableName") {
    val dbSettings = DbSettings("org.h2.Driver", "jdbc:h2:mem:" + UUID.randomUUID(), "", "")
    val storage = new JdbcCrudStorage(dbSettings, fnc, None)
    storage.formatTableName("RecordTagRelation") shouldBe "RECORD_TAG_RELATION"
  }

}
