package xtract.jdbc

import java.sql.{ResultSet, Types => SqlTypes}

import xtract.BuiltInConverters.IntegerToInt
import xtract._


object ResultSetReader extends Reader[ResultSet] {
  def get(data: ResultSet, key: String): Option[Any] = {
    val metaData = data.getMetaData
    val columnIndex = data.findColumn(key)
    val columnType = metaData.getColumnType(columnIndex)
    val result = columnType match {
      case SqlTypes.INTEGER => data.getInt(columnIndex)
      case SqlTypes.BIGINT => data.getLong(columnIndex)
      case SqlTypes.VARCHAR => data.getString(columnIndex)
      case SqlTypes.TIMESTAMP => data.getTimestamp(columnIndex)
    }
    Some(result)
  }
}

object ResultSetParams extends ReadParams[ResultSet](
  reader = ResultSetReader,
  layout = FlatLayout("__"),
  thns = SamePackageTypeHintNamingStrategy,
  fnc = FieldNamingConvention(LowerCase, Underscore),
  converters = Seq(IntegerToInt)
)