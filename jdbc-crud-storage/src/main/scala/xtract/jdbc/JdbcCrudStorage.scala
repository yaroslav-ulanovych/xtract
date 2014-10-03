package xtract.jdbc

import java.sql.{Statement, Connection, PreparedStatement}

import com.mchange.v2.c3p0.ComboPooledDataSource
import com.typesafe.scalalogging.StrictLogging
import xtract._
import xtract.query._

import scala.collection.mutable.{HashMap => MutableHashMap}
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag


case class DbSettings(driver: String, url: String, user: String, password: String)

class JdbcCrudStorage(
  settings: DbSettings,
  fnc: FieldNamingConvention,
  schema: Option[String] = None,
  converters: Seq[Converter[_, _]] = Seq()
) extends CrudStorage with StrictLogging {
  import JdbcCrudStorage._

  val functions = settings.driver match {
    case "org.h2.Driver" => H2SpecificFunctions
    case "org.postgresql.Driver" => PostgresSpecificFunctions
  }
  import functions._

  private val cpds = new ComboPooledDataSource()
  cpds.setDriverClass(settings.driver)
  cpds.setJdbcUrl(settings.url)
  cpds.setUser(settings.user)
  cpds.setPassword(settings.password)
  cpds.setMinPoolSize(1)
  cpds.setMaxPoolSize(1)
  cpds.setCheckoutTimeout(3000)
  cpds.setAcquireRetryAttempts(0)

  private val connectionTL = new ThreadLocal[Connection]

  protected def connection = Option(connectionTL.get()).getOrElse(throw new RuntimeException("no connection, are you in transaction?"))

  def inTransaction[T](doWork: => T): T = {
    val connection = cpds.getConnection
    val autoCommit = connection.getAutoCommit
    connection setAutoCommit false

    if (schema.isDefined) {
      val stmt = connection.createStatement()
      val setSchemaCommand = makeSetSchemaCommand(settings.driver, schema.get)
      stmt.execute(setSchemaCommand)
      stmt.close()
    }

    connectionTL set connection
    try {
      val result = doWork
      connection.commit()
      result
    } finally {
      connection setAutoCommit autoCommit
      connection.close()
      connectionTL.remove()
    }
  }

  def getReader = ResultSetReader

  val writeParams = WriteParams(
    writer = MapWriter,
    reader = MapReader,
    fnc = fnc,
    thns = SamePackageTypeHintNamingStrategy,
    layout = layout,
    allowedClasses = DefaultWriteParams.allowedClasses,
    converters = converters
  )

  val readParams = ReadParams(
    reader = ResultSetReader,
    layout = layout,
    thns = SamePackageTypeHintNamingStrategy,
    fnc = fnc,
    converters = converters
  )

  def close = cpds.close()


  def create[T <: Entity](obj: T) {
    val data = write(obj, writeParams)

    val tableName = getTableName(obj)

    val (sql, params) = JdbcCrudStorage.makeInsertQuery(tableName, List(data), escape)

    val stmt = connection.prepareStatement(sql)

    params.zipWithIndex.foreach(kv => {
      val value = kv._1
      val index = kv._2 + 1
      JdbcCrudStorage.setParameter(stmt, index, value)
    })

    executeStatement(sql, stmt.execute())

    stmt.close()
  }

  def insert[T <: Entity](xs: Traversable[T]) {
    xs.headOption match {
      case Some(head) => {
        val data = xs.map(x => write(x, writeParams))

        val tableName = getTableName(head)

        val (sql, params) = JdbcCrudStorage.makeInsertQuery(tableName, data, escape)

        val stmt = connection.prepareStatement(sql)

        params.zipWithIndex.foreach(kv => {
          val value = kv._1
          val index = kv._2 + 1
          JdbcCrudStorage.setParameter(stmt, index, value)
        })

        executeStatement(sql, stmt.execute())

        stmt.close()
      }
      case None =>
    }
  }

//  def create[T <: Entity with Id](entity: T): T = {
//    val data = entity.write(writeParams)
//    val sql = JdbcCrudStorage.makeInsertQuery(entity.className, data)
//    println(sql)
//
//    val stmt = connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)
//
//    data.values.zipWithIndex.foreach(kv => {
//      val value = kv._1
//      val index = kv._2 + 1
//      JdbcCrudStorage.setParameter(stmt, index, value)
//    })
//
//    executeStatement(sql, stmt.execute())
//
//    // get the auto-generated id
//    val rs = stmt.getGeneratedKeys
//    rs.next()
//    xtract.read.reado(List(entity.id), rs, ResultSetParams)
//    rs.close()
//
//    stmt.close()
//
//    entity
//  }

  def read[T <: Entity with Id : Manifest](id: T#Id): Option[T] = {
    ???
//    val entity = manifest.runtimeClass.newInstance().asInstanceOf[T]
//    val fnc = LowerCase.delimitedBy(Underscore)
//    val idFieldName = fnc.apply(entity.id.qname)
//    val sql = s"select * from ${entity.className} where $idFieldName = ?"
//    val stmt = connection.prepareStatement(sql)
//    id match {
//      case value: Int => stmt.setInt(1, value)
//      case value: Long => stmt.setLong(1, value)
//      case value: String => stmt.setString(1, value)
//    }
//
//    val rs = executeStatement(sql, stmt.executeQuery())
//
//    val result = if (rs.next()) {
//      xtract.read.reado(entity.fields, rs, ResultSetParams)
//      Some(entity)
//    } else {
//      None
//    }
//
//    rs.close()
//    stmt.close()
//
//    result
  }

  def update[T <: Entity with Id](entity: T) = new { def set(f: (T) => Unit) {
    val data1 = entity.write(writeParams)
    f(entity)
    val data2 = entity.write(writeParams)
    val data = data2.filterNot {
      case (k, v) => data1.contains(k) && data1(k) == v
    }
    val idFieldName = ??? //ResultSetParams.getFieldName(entity.id)
    val sql = s"update ${entity.className} set ${data.map(x => x._1 + " = ?").mkString(", ")} where $idFieldName = ?"
    val stmt = connection.prepareStatement(sql)
    data.values.zipWithIndex.foreach(kv => {
      val value = kv._1
      val index = kv._2 + 1
      JdbcCrudStorage.setParameter(stmt, index, value)
    })
    JdbcCrudStorage.setParameter(stmt, data.size + 1, entity.id())

    executeStatement(sql, stmt)

    stmt.close()
  }}

  def getTableName[T <: Entity](obj: T): String = {
    val s = obj match {
      case obj: Obj => obj.className
      case obj: AbstractObj => obj.abstractClassName
    }
    formatTableName(s)
  }
  
  def formatTableName(s: String): String = {
    writeParams.fnc.apply(Utils.splitFieldNameIntoParts(s))
  }

  def getTableName[T <: Entity](klass: Class[T]): String = {
    formatTableName(klass.getSimpleName)
  }

  def select[T <: Obj, U <: Uniqueness](query: Query[T, U])(implicit qr: QueryResult[U]): qr.Result[T] = {
    qr match {
      case x: UniqueQueryResult => selectOne(query).asInstanceOf[qr.Result[T]]
      case x: NotUniqueQueryResult => selectMany(query).asInstanceOf[qr.Result[T]]
    }
  }

  def selectOne[T <: Obj, U <: Uniqueness](query: Query[T, U]): Option[T] = {
    val (where, values) = JdbcCrudStorage.makeWhere(query.clauses, fnc, escape)
    val sql = s"select * from ${escape(getTableName(query.klass))} ${where}"
    val stmt = connection.prepareStatement(sql)
    values.zipWithIndex.map(x => JdbcCrudStorage.setParameter(stmt, x._2 + 1, x._1))

    val rs = executeStatement(sql, stmt.executeQuery())

    val result = if (rs.next()) {
      val obj = xtract.read.read1(query.klass, rs, readParams)
      if (rs.next()) throw new RuntimeException("expected unique result")
      Some(obj)
    } else {
      None
    }

    rs.close()
    stmt.close()

    result
  }

  def selectMany[T <: Obj, U <: Uniqueness](query: Query[T, U])(implicit dummy: DummyImplicit): List[T] = {
    val (where, values) = JdbcCrudStorage.makeWhere(query.clauses, fnc, escape)
    val sql = s"select * from ${escape(getTableName(query.klass))} ${where}"
    val stmt = connection.prepareStatement(sql)
    values.zipWithIndex.map(x => JdbcCrudStorage.setParameter(stmt, x._2 + 1, x._1))

    val rs = executeStatement(sql, stmt.executeQuery())

    val result = ArrayBuffer[T]()

    while (rs.next) {
      val obj = xtract.read.read1(query.klass, rs, readParams)
      result += obj
    }

    if (rs.next()) throw new RuntimeException("expected unique result")

    rs.close()
    stmt.close()

    result.toList
  }

  def delete[T <: Entity: Manifest]() {
    val entity = manifest.runtimeClass.newInstance().asInstanceOf[T]
    val sql = s"""delete from ${entity.className}"""
    val stmt = connection.createStatement()

    executeStatement(sql, stmt.executeUpdate(sql))

    stmt.close
  }


  def delete[T <: Entity : Manifest, U <: Uniqueness](where: Query[T, U]) {
    val entity = where.meta.getClass.newInstance().asInstanceOf[T]
    val sql = s"delete from ${entity.className} where ${???}"
    val stmt = connection.createStatement()
    executeStatement(sql, stmt.executeUpdate(sql))
    stmt.close()
  }

  private def executeStatement[T](sql: String, f: => T): T = {
    val before = System.currentTimeMillis()

    try {
      f
    } finally {
      val after = System.currentTimeMillis()
      val time = after - before
      logger.debug(sql + ", " + time + "ms")
    }
  }
}



object JdbcCrudStorage {
//  def makeInsertQuery(tableName: String, data: MutableHashMap[String, Any], escape: String => String): String = {
//    val tableNameEscaped = escape(tableName)
//    val fieldNamesEscaped = data.keys.map(escape)
//
//    s"insert into ${tableNameEscaped}(${fieldNamesEscaped.mkString(", ")}) values(${data.keys.toSeq.map(x => "?").mkString(", ")})"
//  }

  def makeInsertQuery(tableName: String, data: Traversable[MutableHashMap[String, Any]], escape: String => String): (String, Seq[Any]) = {
    val buffer = ArrayBuffer[Any]()
    val tableNameEscaped = escape(tableName)
    val fieldNamesEscaped = data.head.keys.map(escape)

    val sql = s"insert into ${tableNameEscaped}(${fieldNamesEscaped.mkString(", ")}) values ${data.map(data => s"(${data.map({case (k, v) => buffer += v; "?"}).mkString(", ")})").mkString(", ")}"
    (sql, buffer)
  }

  import xtract.query._

  def makeWhere(clauses: List[QueryClause[_]], fnc: FieldNamingConvention, escape: String => String): (String, List[Any]) = {
    val values = ArrayBuffer[Any]()
    def getFieldName(field: Entity#Field[_]) = escape(getFQName(field, fnc))
    val sql = "where " + clauses.map(_ match {
//      case IsClause(field, klass) => {
//        values += klass.newInstance().typeDiscriminator
//        s"""${field.qname + "_type"} = ?"""
//      }
      case SimpleFieldEqClause(field, value) => {
        values += value
        s"""${getFieldName(field)} = ?"""
      }
      case SimpleFieldInClause(field, xs) => {
        values ++= xs
        // we can't do xs.map(x => "?"), cause xs may be set
        s"""${getFieldName(field)} in (${Array.fill(xs.size)("?").mkString(", ")})"""
      }
//      case LinkFieldEqClause(field, value) => {
//        values += value
//        s"""${field.fqname} = ?"""
//      }
//      case CustomFieldEqClause(field, value) => {
//        values += field.serialize(value)
//        s"""${field.fqname} = ?"""
//      }
    }).mkString(" and ")

    if (clauses.nonEmpty) {
      (sql, values.toList)
    } else {
      ("", values.toList)
    }

  }


  def getFQName(field: Entity#Field[_], fnc: FieldNamingConvention): String = {
    field.fqname(SamePackageTypeHintNamingStrategy).map(fnc.apply).mkString(layout.separator)
  }

  def setParameter(stmt: PreparedStatement, index: Int, value: Any) {
    value match {
      case value: Int => stmt.setInt(index, value)
      case value: Long => stmt.setLong(index, value)
      case value: String => stmt.setString(index, value)
      case value: java.sql.Timestamp => stmt.setTimestamp(index, value)
      case value: java.util.Date => stmt.setTimestamp(index, new java.sql.Timestamp(value.getTime))
      case any => throw new UnsupportedOperationException(s"${any.getClass.getCanonicalName} isn't supported")
    }
  }

  val layout = FlatLayout("__")

//  val writeParams = WriteParams(
//    writer = MapWriter,
//    reader = MapReader,
//    fnc = fnc,
//    thns = SamePackageTypeHintNamingStrategy,
//    layout = layout
//  )
}