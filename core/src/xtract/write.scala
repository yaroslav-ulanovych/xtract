package xtract

object write extends Object with FieldTypeShortcuts {
  def apply[T](obj: Any, params: WriteParams[T] = DefaultWriteParams): T = {
    val data = params.writer.create
    write1(obj, data, params)
    data
  }

  def write1[T](obj: Any, data: T, params: WriteParams[T]) {
    obj match {
      case obj: AbstractObj => {
        val typeHint = params.thns.getTypeHint(obj)
        val key = params.layout.makeKey(List("type"), params.fnc)
        params.writer.put(data, key, typeHint.render(CamelCase.noDelimiter))
        val (data2, layout) = params.layout.dive2(data, typeHint.render(params.fnc), params)
        writeObj(obj, data2, params + layout)
      }
      case obj: Obj => {
        writeObj(obj, data, params)
      }
    }
  }

  def writeObj[T <: Entity, U](obj: T, data: U, params: WriteParams[U]): U = {
    for(field <- obj.fields) {
      field.asInstanceOf[Entity#Field[_]] match {
        case field_ : SimpleField => {
          val key = params.layout.makeKey(field.getName().words, params.fnc)
          params.writer.put(data, key, field())
        }
        case field_ : obj.EmbeddedConcreteField[_] => {
          val field = field_.asInstanceOf[obj.EmbeddedConcreteField[Obj]]
          val key = params.layout.makeKey(field.getName().words, params.fnc)
          val (data2, layout) = params.layout.dive1(data, key, params)
          val entity = field()
          writeObj(entity, data2, params + layout)
        }
      }
    }
    data
  }
}
