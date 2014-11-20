package xtract



trait LayoutOld extends Object {
  def dive1[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, LayoutOld)]]
  def dive2[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, LayoutOld)]]
//  def dive2[T](data: T, key: String, params: WriteParams[T]): (T, LayoutOld)
  def makeKey(field: List[String], fnc: FieldNamingConvention): String
}

object NestedLayoutOld extends LayoutOld {
  def dive1[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, LayoutOld)]] = {
    params.reader.get(data, key) match {
      case Some(v) if params.reader.accepts(v.getClass) => Some(Right((v.asInstanceOf[T], this)))
      case Some(v) => Some(Left(v))
      case None => None
    }
  }

  def dive2[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, LayoutOld)]] = {
    params.reader.get(data, "args") match {
      case Some(v) if params.reader.accepts(v.getClass) => Some(Right((v.asInstanceOf[T], this)))
      case Some(v) => Some(Left(v))
      case None => None
    }
  }

  def dive2[T](data: T, key: String, params: WriteParams[T]): (T, LayoutOld) = {
    val data2 = params.writer.create
    params.writer.put(data, "args", data2)
    (data2, this)
  }

  def makeKey(field: List[String], fnc: FieldNamingConvention): String = {
    fnc.apply(field)
  }
}

case class FlatLayoutOld(separator: String, prefix: String = "") extends LayoutOld {
  def dive1[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, LayoutOld)]] = {
    Some(Right((data, FlatLayoutOld(separator, prefix + key + separator))))
  }

  def dive2[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, LayoutOld)]] = {
    dive1(data, key, params)
  }

  def dive2[T](data: T, key: String, params: WriteParams[T]): (T, LayoutOld) = {
    (data, FlatLayoutOld(separator, prefix + key + separator))
  }

  def makeKey(field: List[String], fnc: FieldNamingConvention): String = {
    prefix + fnc.apply(field)
  }
}

trait FieldsLayout {
  def dive[T](data: T, key: String, typeHint: String, params: WriteParams[T]): (T, Writer[T])
}

object SingleLevelFieldsLayout extends FieldsLayout {
  def dive[T](data: T, key: String, typeHint: String, params: WriteParams[T]): (T, Writer[T]) = {
    params.diver.dive(data, key, params)
  }
}

object TypeHintFieldsLayout extends FieldsLayout {
  def dive[T](data: T, key: String, typeHint: String, params: WriteParams[T]): (T, Writer[T]) = {
    val (data2, writer) = params.diver.dive(data, key, params)
    params.diver.dive(data2, typeHint, params + writer)
  }
}

