package xtract.jdbc

import java.sql.{Statement, Connection, PreparedStatement}

import com.mchange.v2.c3p0.ComboPooledDataSource
import xtract._
import xtract.query._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag


case class DbSettings(driver: String, url: String, user: String, password: String)

class JdbcCrudStorage(settings: DbSettings) extends CrudStorage {

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


  private val writeParams = WriteParams(
    writer = MapWriter,
    reader = MapReader,
    fnc = LowerCase.delimitedBy(Underscore),
    thns = SamePackageTypeHintNamingStrategy,
    layout = FlatLayout("__")
  )

  def create[T <: Entity](obj: T)(implicit dummy: DummyImplicit) {
    val data = write(obj, writeParams)

    val tableName = getTableName(obj)

    val sql = JdbcCrudStorage.makeInsertQuery(tableName, data)

    val stmt = connection.prepareStatement(sql)

    data.values.zipWithIndex.foreach(kv => {
      val value = kv._1
      val index = kv._2 + 1
      JdbcCrudStorage.setParameter(stmt, index, value)
    })

    executeStatement(sql, stmt.execute())

    stmt.close()
  }

  def create[T <: Entity with Id](entity: T): T = {
    val data = entity.write(writeParams)
    val sql = JdbcCrudStorage.makeInsertQuery(entity.className, data)
    println(sql)

    val stmt = connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)

    data.values.zipWithIndex.foreach(kv => {
      val value = kv._1
      val index = kv._2 + 1
      JdbcCrudStorage.setParameter(stmt, index, value)
    })

    executeStatement(sql, stmt.execute())

    // get the auto-generated id
    val rs = stmt.getGeneratedKeys
    rs.next()
    xtract.read.reado(List(entity.id), rs, ResultSetParams)
    rs.close()

    stmt.close()

    entity
  }

  def read[T <: Entity with Id : Manifest](id: T#Id): Option[T] = {
    val entity = manifest.runtimeClass.newInstance().asInstanceOf[T]
    val fnc = LowerCase.delimitedBy(Underscore)
    val idFieldName = fnc.apply(entity.id.qname)
    val sql = s"select * from ${entity.className} where $idFieldName = ?"
    val stmt = connection.prepareStatement(sql)
    id match {
      case value: Int => stmt.setInt(1, value)
      case value: Long => stmt.setLong(1, value)
      case value: String => stmt.setString(1, value)
    }

    val rs = executeStatement(sql, stmt.executeQuery())

    val result = if (rs.next()) {
      xtract.read.reado(entity.fields, rs, ResultSetParams)
      Some(entity)
    } else {
      None
    }

    rs.close()
    stmt.close()

    result
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
    obj match {
      case obj: Obj => obj.className
      case obj: AbstractObj => obj.abstractClassName
    }
  }

  def select[T <: Entity: ClassTag]: List[T] = {
    val klass = implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[Entity]]
    val sql = s"select * from ${klass.getSimpleName}"
    val stmt = connection.prepareStatement(sql)

    val rs = executeStatement(sql, stmt.executeQuery())

    val result = ArrayBuffer[T]()

    while (rs.next()) {
      result += xtract.read[T].from(rs)(ResultSetParams)
    }

    rs.close()
    stmt.close()

    result.toList
  }

  def select[T <: Entity](query: Query[T]): Option[T] = {
    val entity = query.meta.getClass.asInstanceOf[Class[T]].newInstance()
    val (where, values) = JdbcCrudStorage.makeWhere(query.clauses)
    val sql = s"select * from ${entity.className} where ${where}"
    val stmt = connection.prepareStatement(sql)
    values.zipWithIndex.map(x => JdbcCrudStorage.setParameter(stmt, x._2 + 1, x._1))

    val rs = executeStatement(sql, stmt.executeQuery())

    val result = if (rs.next()) {
      xtract.read.reado(entity.fields, rs, ResultSetParams)
      Some(entity)
    } else {
      None
    }

    if (rs.next()) throw new RuntimeException("expected unique result")

    rs.close()
    stmt.close()

    result
  }


  def delete[T <: Entity: Manifest]() {
    val entity = manifest.runtimeClass.newInstance().asInstanceOf[T]
    val sql = s"""delete from ${entity.className}"""
    val stmt = connection.createStatement()

    executeStatement(sql, stmt.executeUpdate(sql))

    stmt.close
  }


  def delete[T <: Entity : Manifest](where: Query[T]) {
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
      println(sql + ", " + time + "ms")
    }
  }
}



object JdbcCrudStorage {
  def makeInsertQuery(table: String, data: mutable.HashMap[String, Any]): String ={
    if (!data.isEmpty) {
      s"insert into ${table}(${data.keys.mkString(", ")}) values(${data.keys.toSeq.map(x => "?").mkString(", ")})"
    } else {
      s"insert into ${table} default values"
    }
  }

  import xtract.query._

  def makeWhere(clauses: List[QueryClause]): (String, List[Any]) = {
    val values = ArrayBuffer[Any]()
    val sql = clauses.map(_ match {
      case IsClause(field, klass) => {
        values += klass.newInstance().typeDiscriminator
        s"""${field.qname + "_type"} = ?"""
      }
      case SimpleFieldEqClause(field, value) => {
        values += value
        s"""${field.fqname} = ?"""
      }
      case LinkFieldEqClause(field, value) => {
        values += value
        s"""${field.fqname} = ?"""
      }
      case CustomFieldEqClause(field, value) => {
        values += field.serialize(value)
        s"""${field.fqname} = ?"""
      }
    }).mkString(" and ")
    (sql, values.toList)
  }

  def setParameter(stmt: PreparedStatement, index: Int, value: Any) {
    value match {
      case value: Int => stmt.setInt(index, value)
      case value: Long => stmt.setLong(index, value)
      case value: String => stmt.setString(index, value)
      case value: java.util.Date => stmt.setTimestamp(index, new java.sql.Timestamp(value.getTime))
      case any => throw new UnsupportedOperationException(s"${any.getClass.getCanonicalName} isn't supported")
    }
  }
}