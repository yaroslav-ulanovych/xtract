package xtract


trait Layout extends Object {
  def dive1[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, Layout)]]
  def dive2[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, Layout)]]
  def dive1[T](data: T, key: String, params: WriteParams[T]): (T, Layout)
  def dive2[T](data: T, key: String, params: WriteParams[T]): (T, Layout)
  def makeKey(field: List[String], fnc: FieldNamingConvention): String
}

object NestedLayout extends Layout {
  def dive1[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, Layout)]] = {
    params.reader.get(data, key) match {
      case Some(v) if params.reader.accepts(v.getClass) => Some(Right((v.asInstanceOf[T], this)))
      case Some(v) => Some(Left(v))
      case None => None
    }
  }

  def dive2[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, Layout)]] = {
    params.reader.get(data, "args") match {
      case Some(v) if params.reader.accepts(v.getClass) => Some(Right((v.asInstanceOf[T], this)))
      case Some(v) => Some(Left(v))
      case None => None
    }
  }

  def dive1[T](data: T, key: String, params: WriteParams[T]): (T, Layout) = {
    val data2 = params.writer.create
    params.writer.put(data, key, data2)
    (data2, this)
  }

  def dive2[T](data: T, key: String, params: WriteParams[T]): (T, Layout) = {
    val data2 = params.writer.create
    params.writer.put(data, "args", data2)
    (data2, this)
  }

  def makeKey(field: List[String], fnc: FieldNamingConvention): String = {
    fnc.apply(field)
  }
}

case class FlatLayout(separator: String, prefix: String = "") extends Layout {
  def dive1[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, Layout)]] = {
    Some(Right((data, FlatLayout(separator, prefix + key + separator))))
  }

  def dive2[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, Layout)]] = {
    dive1(data, key, params)
  }

  def dive1[T](data: T, key: String, params: WriteParams[T]): (T, Layout) = {
    (data, FlatLayout(separator, prefix + key + separator))
  }

  def dive2[T](data: T, key: String, params: WriteParams[T]): (T, Layout) = {
    dive1(data, key, params)
  }

  def makeKey(field: List[String], fnc: FieldNamingConvention): String = {
    prefix + fnc.apply(field)
  }
}