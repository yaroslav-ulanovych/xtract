package xtract

trait TypeHintLocation {
  def get[T](field: List[String], data: T, params: ReadParams[T]): Option[Either[Any, String]]
  def put[T](data: T, data2: T, field: List[String], typeHint: String, params: WriteParams[T])
}

case class BelowTypeHintLocation(typeHintFieldName: List[String]) extends TypeHintLocation {
  def this(typeHintFieldName: String) = this(Utils.splitFieldNameIntoParts(typeHintFieldName))

  def get[T](field: List[String], data: T, params: ReadParams[T]): Option[Either[Any, String]] = {
    val key = params.layout.makeKey(field, None, params.fnc)
    params.layout.dive(data, key, params) match {
      case Some(Right((data, layout))) => {
        val key = layout.makeKey(typeHintFieldName, None, params.fnc)
        params.reader.get(data, key) match {
          case Some(s: String) => Some(Right(s))
          case Some(v) => Some(Left(v))
          case None => None
        }
      }
      case Some(Left(v)) => Some(Left(v))
      case None => None
    }
  }

  def put[T](data: T, data2: T, field: List[String], typeHint: String, params: WriteParams[T]) {
    val key = params.layout.makeKey(field, None, params.fnc)
    val (newData, newLayout) = params.layout.dive(data, key, params)
    params.writer.put(newData, newLayout.makeKey(typeHintFieldName, None, params.fnc), typeHint)
  }
}

object BelowTypeHintLocation {
  def apply(typeHintFieldName: String): BelowTypeHintLocation = BelowTypeHintLocation(Utils.splitFieldNameIntoParts(typeHintFieldName))
}

case class NearTypeHintLocation(postfix: List[String]) extends TypeHintLocation {
  def get[T](field: List[String], data: T, params: ReadParams[T]): Option[Either[Any, String]] = {
    val key = params.layout.makeKey(field ::: postfix, None, params.fnc)
    params.reader.get(data, key) match {
      case Some(s: String) => Some(Right(s))
      case Some(v) => Some(Left(v))
      case None => None
    }
  }

  def put[T](data: T, data2: T, field: List[String], typeHint: String, params: WriteParams[T]) {
    val key = params.layout.makeKey(field ::: postfix, None, params.fnc)
    params.writer.put(data, key, typeHint)
  }
}

object NearTypeHintLocation {
  def apply(postfix: String): NearTypeHintLocation = NearTypeHintLocation(Utils.splitFieldNameIntoParts(postfix))
}