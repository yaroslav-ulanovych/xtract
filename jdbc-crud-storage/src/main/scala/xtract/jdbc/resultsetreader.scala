package xtract.jdbc

import java.sql.{ResultSet, Types => SqlTypes}

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
