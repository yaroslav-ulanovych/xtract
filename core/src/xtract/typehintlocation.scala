package xtract

trait TypeHintLocationStrategy {
  def getTypeHint[T](data: T, field: Option[FieldName], params: ReadParams[T]): Option[Either[Any, String]]
  def putTypeHint[T](data: T, field: Option[FieldName], typeHint: String, params: WriteParams[T])
}

case class BelowTypeHintLocationStrategy(typeHintFieldName: FieldName) extends TypeHintLocationStrategy {
  def getTypeHint[T](data: T, field: Option[FieldName], params: ReadParams[T]): Option[Either[Any, String]] = {
    field match {
      case Some(field) => {
        val key = params.fnc.apply(field)
        params.diver.dive(data, key, params) match {
          case Some(Right((data, reader))) => {
            reader.get(data, params.fnc.apply(typeHintFieldName)) match {
              case Some(typeHint: String) => Some(Right(typeHint))
              case Some(value) => Some(Left(value))
              case None => None
            }
          }
          case Some(Left(_)) => None
          case None => None
        }
      }
      case None => {
        throw new Exception(s"${getClass.getSimpleName} makes no sense for standalone polymorphic objects")
      }
    }
  }

  def putTypeHint[T](data: T, field: Option[FieldName], typeHint: String, params: WriteParams[T]) {
    ???
//    val (nestedData, writer) = params.diver.dive(data, key, params)
//    writer.put(nestedData, params.fnc.apply(typeHintFieldName), typeHint)
  }
}

case class NearTypeHintLocationStrategy(typeHintFieldName: FieldName) extends TypeHintLocationStrategy {
  def getTypeHint[T](data: T, field: Option[FieldName], params: ReadParams[T]): Option[Either[Any, String]] = {
    val key = params.fnc.apply(field.map(_ + typeHintFieldName).getOrElse(typeHintFieldName))
    params.reader.get(data, key) match {
      case Some(typeHint: String) => Some(Right(typeHint))
      case Some(value) => Some(Left(value))
      case None => None
    }
  }

  def putTypeHint[T](data: T, field: Option[FieldName], typeHint: String, params: WriteParams[T]) {
    val key = params.fnc.apply(field.map(_ + typeHintFieldName).getOrElse(typeHintFieldName))
    params.writer.put(data, key, typeHint)
  }
}