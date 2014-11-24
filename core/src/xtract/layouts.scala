package xtract



trait LayoutOld extends Object {
//  def dive1[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, LayoutOld)]]
//  def dive2[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, LayoutOld)]]
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

  def makeKey(field: List[String], fnc: FieldNamingConvention): String = {
    prefix + fnc.apply(field)
  }
}

trait FieldsLocation {
  def dive[T](data: T, field: Option[FieldName], typeHint: String, params: ReadParams[T]): Option[Either[Any, (T, Reader[T])]]
  def dive[T](data: T, field: Option[FieldName], typeHint: String, params: WriteParams[T]): (T, Writer[T])
}

object SimpleFieldsLocation extends FieldsLocation {
  def dive[T](data: T, field: Option[FieldName], typeHint: String, params: ReadParams[T]): Option[Either[Any, (T, Reader[T])]] = {
    field match {
      case Some(field) => params.diver.dive(data, field.render(params.fnc), params)
      case None => Some(Right((data, params.reader)))
    }
  }

  def dive[T](data: T, field: Option[FieldName], typeHint: String, params: WriteParams[T]): (T, Writer[T]) = {
    field match {
      case Some(field) => params.diver.dive(data, field.render(params.fnc), params)
      case None => (data, params.writer)
    }
  }
}

object TypeHintFieldsLocation extends FieldsLocation {
  def dive[T](data: T, field: Option[FieldName], typeHint: String, params: ReadParams[T]): Option[Either[Any, (T, Reader[T])]] = ???

  def dive[T](data: T, field: Option[FieldName], typeHint: String, params: WriteParams[T]): (T, Writer[T]) = {
    field match {
      case Some(field) => {
        val key = field.render(params.fnc)
        val (data2, writer) = params.diver.dive(data, key, params)
        params.diver.dive(data2, typeHint, params + writer)
      }
      case None => {
        params.diver.dive(data, typeHint, params)
      }
    }
  }
}

case class DedicatedFieldsLocation(fieldsFieldName: FieldName) extends FieldsLocation {
  def dive[T](data: T, field: Option[FieldName], typeHint: String, params: ReadParams[T]): Option[Either[Any, (T, Reader[T])]] = {
    field match {
      case Some(field) => {
        val key = field.render(params.fnc)
        params.diver.dive(data, key, params) match {
          case Some(Right((data2, reader2))) => {
            params.diver.dive(data2, fieldsFieldName.render(params.fnc), params + reader2)
          }
          case Some(Left(value)) => Some(Left(value))
          case None => None
        }
      }
      case None => {
        params.diver.dive(data, fieldsFieldName.render(params.fnc), params)
      }
    }

  }

  def dive[T](data: T, field: Option[FieldName], typeHint: String, params: WriteParams[T]): (T, Writer[T]) = {
    field match {
      case Some(field) => {
        val key = field.render(params.fnc)
        val (data2, writer) = params.diver.dive(data, key, params)
        params.diver.dive(data2, fieldsFieldName.render(params.fnc), params + writer)
      }
      case None => {
        params.diver.dive(data, fieldsFieldName.render(params.fnc), params)
      }
    }
  }
}

