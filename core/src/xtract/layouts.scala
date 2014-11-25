package xtract

trait FieldsLocation {
  def dive[T](data: T, field: Option[FieldName], typeHint: String, params: ReadParams[T]): (T, Reader[T])
  def dive[T](data: T, field: Option[FieldName], typeHint: String, params: WriteParams[T]): (T, Writer[T])
}

object SimpleFieldsLocation extends FieldsLocation {
  def dive[T](data: T, field: Option[FieldName], typeHint: String, params: ReadParams[T]): (T, Reader[T]) = {
    field match {
      case Some(field) => params.diver.dive(data, field.render(params.fnc), params)
      case None => (data, params.reader)
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
  def dive[T](data: T, field: Option[FieldName], typeHint: String, params: ReadParams[T]): (T, Reader[T]) = {
    field match {
      case Some(field) => {
        val key = field.render(params.fnc)
        val (data2, reader2) = params.diver.dive(data, key, params)
        params.diver.dive(data2, typeHint, params + reader2)
      }
      case None => {
        params.diver.dive(data, typeHint, params)
      }
    }
  }

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
  def dive[T](data: T, field: Option[FieldName], typeHint: String, params: ReadParams[T]): (T, Reader[T]) = {
    field match {
      case Some(field) => {
        val key = field.render(params.fnc)
        val (data2, reader2) = params.diver.dive(data, key, params)
        params.diver.dive(data2, fieldsFieldName.render(params.fnc), params + reader2)
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

