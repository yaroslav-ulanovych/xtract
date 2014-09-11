package xtract

trait Layout extends Object {
  def makeKey(field: List[String], typeHint: Option[String], fnc: FieldNamingConvention): String
  def dive[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, Layout)]]
  def dive[T](data: T, key: String, params: WriteParams[T]): (T, Layout)
}

object NestedLayout extends Layout {
  def dive[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, Layout)]] = {
    params.reader.get(data, key) match {
      case Some(v) if params.reader.accepts(v.getClass) => Some(Right((v.asInstanceOf[T], this)))
      case Some(v) => Some(Left(v))
      case None => None
    }
  }

  def makeKey(field: List[String], typeHint: Option[String], fnc: FieldNamingConvention): String = {
    fnc.apply(field)
  }

  def dive[T](data: T, key: String, params: WriteParams[T]): (T, Layout) = {
    val data2 = params.writer.create
    params.reader.get(data, key) match {
      case Some(data) => (data.asInstanceOf[T], this)
      case None => {
        params.writer.put(data, key, data2)
        (data2, this)
      }
    }
  }
}

case class FlatLayout(separator: String, prefix: String = "") extends Layout {
  def dive[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, Layout)]] = {
    Some(Right((data, FlatLayout(separator, key + separator))))
  }

  def makeKey(field: List[String], typeHint: Option[String], fnc: FieldNamingConvention): String = {
    prefix + fnc.apply(field ::: typeHint.toList)
  }

  def dive[T](data: T, key: String, params: WriteParams[T]): (T, Layout) = {
    (data, FlatLayout(separator, key + separator))
  }
}