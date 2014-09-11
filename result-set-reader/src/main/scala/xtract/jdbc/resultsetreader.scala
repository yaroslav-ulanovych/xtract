package xtract

import java.sql.{Types => SqlTypes, ResultSet}


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
  thl = NearTypeHintLocation(List("type")),
  fnc = FieldNamingConvention(LowerCase, Underscore),
  converters = Seq()
)