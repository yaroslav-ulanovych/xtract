package xtract

trait TypeHintLocationStrategy {
  def getTypeHint[T](data: T, field: Option[FieldName], params: ReadParams[T]): String
  def putTypeHint[T](data: T, field: Option[FieldName], typeHint: String, params: WriteParams[T])
}

case class BelowTypeHintLocationStrategy(typeHintFieldName: FieldName) extends TypeHintLocationStrategy {
  def getTypeHint[T](data: T, field: Option[FieldName], params: ReadParams[T]): String = {
    val typeHintKey = params.fnc.apply(typeHintFieldName)
    field match {
      case Some(field) => {
        val (data2, reader2) = params.diver.dive(data, params.fnc.apply(field), params)
        reader2.get_!(data2, typeHintKey) match {
          case typeHint: String => typeHint
          case value => throw BadKeyValueException(data, typeHintKey, value, "string")
        }
      }
      case None => {
        params.reader.get_!(data, typeHintKey) match {
          case typeHint: String => typeHint
          case value => throw BadKeyValueException(data, typeHintKey, value, "string")
        }
      }
    }
  }

  def putTypeHint[T](data: T, field: Option[FieldName], typeHint: String, params: WriteParams[T]) {
    val typeHintKey = typeHintFieldName.render(params.fnc)
    field match {
      case Some(field) => {
        val (data2, writer2) = params.diver.dive(data, params.fnc.apply(field), params)
        writer2.put(data2, typeHintKey, typeHint)
      }
      case None => {
        params.writer.put(data, typeHintKey, typeHint)
      }
    }
  }
}

case class NearTypeHintLocationStrategy(typeHintFieldName: FieldName) extends TypeHintLocationStrategy {
  def getTypeHint[T](data: T, field: Option[FieldName], params: ReadParams[T]): String = {
    val key = params.fnc.apply(field.map(_ + typeHintFieldName).getOrElse(typeHintFieldName))
    params.reader.get_!(data, key) match {
      case typeHint: String => typeHint
      case value => throw BadKeyValueException(data, key, value, "string")
    }
  }

  def putTypeHint[T](data: T, field: Option[FieldName], typeHint: String, params: WriteParams[T]) {
    val key = params.fnc.apply(field.map(_ + typeHintFieldName).getOrElse(typeHintFieldName))
    params.writer.put(data, key, typeHint)
  }
}