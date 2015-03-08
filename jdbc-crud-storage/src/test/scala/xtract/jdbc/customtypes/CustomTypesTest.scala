package xtract.jdbc.customtypes

import java.sql.{Timestamp, DriverManager}
import java.util.UUID

import org.joda.time.{Instant, LocalDateTime}
import org.scalatest.{Matchers, FunSuite}
import xtract._
import xtract.jdbc.{JdbcCrudStorage, DbSettings}

class Note extends Obj {
  val ctime = field[Instant]
}

object TimestampToInstantConverter extends SimpleConverter[Timestamp, Instant](x => Some(new Instant(x)), x => new Timestamp(x.getMillis))

class CustomTypesTest extends FunSuite with Matchers {
  val fnc = UpperCase.delimitedBy(Underscore)

  test("test") {
    val dbSettings = DbSettings("org.h2.Driver", "jdbc:h2:mem:" + UUID.randomUUID(), "", "")
    val conn = DriverManager.getConnection(dbSettings.url)
    val stmt = conn.createStatement()
    stmt.execute("create table note(ctime timestamp);")

    val storage = new JdbcCrudStorage(dbSettings, fnc, None, Seq(TimestampToInstantConverter))

    try {
      storage.inTransaction {
        import xtract.query.QueryDsl._

        val note = new Note

        val time = Instant.now()

        note.ctime := time

        storage.insert(List(note))

        val notes = storage.select(from[Note])

        notes.size shouldBe 1

        notes(0).ctime() shouldBe time
      }
    } finally {
      conn.close()
    }
  }
}
