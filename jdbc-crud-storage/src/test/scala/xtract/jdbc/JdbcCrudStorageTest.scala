package xtract.jdbc

import java.sql.DriverManager
import java.util.UUID

import org.scalatest.{GivenWhenThen, Matchers, FunSuite}
import xtract.query.Query
import xtract.{AbstractObj, Obj}

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
//  test("bla") {
//    val conn = DriverManager.getConnection("jdbc:h2:mem:db1")
//    val stmt = conn.createStatement()
//    stmt.execute("create table account(type varchar, twitter__id varchar, google__id varchar);")
//    val storage = new JdbcCrudStorage(DbSettings("org.h2.Driver", "jdbc:h2:mem:db1", "", ""))
//    storage.inTransaction {
//      storage.create(Obj.create[TwitterAccount](_.id := "twitterId1"))
//      storage.create(Obj.create[GoogleAccount](_.id := "googleId1"))
//      val accounts = storage.select[Account]
//      accounts(0).asInstanceOf[TwitterAccount].id() shouldBe "twitterId1"
//      accounts(1).asInstanceOf[GoogleAccount].id() shouldBe "googleId1"
//    }
//  }

//  test("select by single field") {
//    val dbSettings = DbSettings("org.h2.Driver", "jdbc:h2:mem:db1", "", "")
//    val conn = DriverManager.getConnection(dbSettings.url)
//    val stmt = conn.createStatement()
//    stmt.execute("create table user(id int primary key, name varchar);")
//    stmt.execute("insert into user values (1, 'John');")
//    stmt.execute("insert into user values (2, 'John');")
//    stmt.execute("insert into user values (3, 'Mary');")
//
//    val storage = new JdbcCrudStorage(dbSettings)
//    storage.inTransaction {
//      import xtract.query.QueryDsl._
//      {
//        When("selecting few records")
//        val users = storage.select(from[User].where(_.name eqs "John"))
//
//        Then("get few records")
//        users.length shouldBe 2
//
//        users(0).id() shouldBe 1
//        users(0).name() shouldBe "John"
//
//        users(1).id() shouldBe 2
//        users(1).name() shouldBe "John"
//      }
//
//      {
//        When("selecting single record")
//        val users = storage.select(from[User].where(_.name eqs "Mary"))
//
//        Then("get single record")
//        users.length shouldBe 1
//
//        users(0).id() shouldBe 3
//        users(0).name() shouldBe "Mary"
//      }
//
//      {
//        When("selecting no records")
//        val users = storage.select(from[User].where(_.name eqs "not existing name"))
//
//        Then("get no records")
//        users.length shouldBe 0
//      }
//    }
//  }

  test("select all") {
    val dbSettings = DbSettings("org.h2.Driver", "jdbc:h2:mem:db1", "", "")
    val conn = DriverManager.getConnection(dbSettings.url)
    val stmt = conn.createStatement()
    stmt.execute("create table user(id int primary key, name varchar);")
    stmt.execute("insert into user values (1, 'John');")
    stmt.execute("insert into user values (2, 'Mary');")

    val storage = new JdbcCrudStorage(dbSettings)
    storage.inTransaction {
      import xtract.query.QueryDsl._

      val Some(users) = storage.select(from[User])

      users.length shouldBe 2
    }
  }

  test("select unique and not single field") {
    val dbSettings = DbSettings("org.h2.Driver", "jdbc:h2:mem:db1", "", "")
    val conn = DriverManager.getConnection(dbSettings.url)
    val stmt = conn.createStatement()
    stmt.execute("create table user(id int primary key, name varchar);")
    stmt.execute("insert into user values (1, 'John');")
    stmt.execute("insert into user values (2, 'John');")
    stmt.execute("insert into user values (3, 'Mary');")

    val storage = new JdbcCrudStorage(dbSettings)
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
  }

  test("bla") {
    val dbSettings = DbSettings("org.h2.Driver", "jdbc:h2:mem:db1", "", "")
    val conn = DriverManager.getConnection(dbSettings.url)
    val stmt = conn.createStatement()
    stmt.execute("create table user(id int primary key, name varchar);")
    stmt.execute("create table session(id varchar primary key, user_id int references user(id));")

    val storage = new JdbcCrudStorage(dbSettings)
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
  }
}
