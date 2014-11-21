package xtract

import scala.reflect.ClassTag

object write extends Object with FieldTypeShortcuts {
  def apply[T](obj: Any, params: WriteParams[T] = DefaultWriteParams): T = {
    write1(obj, params)
  }

  def write1[T](obj: Any, params: WriteParams[T]): T = {
    obj match {
      case obj: AbstractObj => {
        class Holder extends Obj {
          val value = embedded[AbstractObj]
        }
        val holder = new Holder
        holder.value := obj
        val data = params.writer.create
        writeObj(holder, data, params)
        params.reader.get(data, params.fnc.apply("value")).get.asInstanceOf[T]
//        val typeHint = params.thns.getTypeHint(obj)
//        val key = params.layoutOld.makeKey(List("type"), params.fnc)
//        params.writer.put(data, key, typeHint.render(CamelCase.noDelimiter))
//        val (data2, layout) = params.layoutOld.dive2(data, typeHint.render(params.fnc), params)
//        writeObj(obj, data2, params + layout)
      }
      case obj: Obj => {
        val data = params.writer.create
        writeObj(obj, data, params)
        data
      }
    }
  }

  def writeObj[T <: Entity, U](obj: T, data: U, params: WriteParams[U]): U = {
    writeObj(obj, obj.fields, data, params)
  }

  def writeObj[T <: Entity, U](obj: T, fields: Seq[Entity#Field[_]], data: U, params: WriteParams[U]): U = {
    for(field <- fields) {
      field.asInstanceOf[Entity#Field[_]] match {
        case field_ : SimpleField => {
          val key = params.layoutOld.makeKey(field.getName().words, params.fnc)
          val value = field()
          val valueClass = value.getClass

          if (params.classAllowed(valueClass)) {
            params.writer.put(data, key, value)
          } else {
            params.findConverterFrom(valueClass) match {
              case Some(converter) => {
                val convertedValue = converter.asInstanceOf[Converter[Any, Any]].convertBack(value)
                params.writer.put(data, key, convertedValue)
              }
              case None => throw OtherWriteException(obj, s"${valueClass.getName} is not allowed in write output")
            }
          }
        }
        case fld: obj.EmbeddedConcreteField[_] => {
          val field = fld.asInstanceOf[obj.EmbeddedConcreteField[Obj]]
          val key = params.fnc.apply(field.getName())
          val (nestedData, writer) = params.diver.dive(data, key, params)
          writeObj(field(), nestedData, params + writer)
        }
        case fld: obj.EmbeddedPolymorphicField[_] => {
          val field = fld.asInstanceOf[obj.EmbeddedPolymorphicField[AbstractObj]]
          val embeddedObj = field()
          val key = params.fnc.apply(field.getName())
          val typeHint = params.thns.getTypeHint(embeddedObj)
          params.thls.putTypeHint(data, key, typeHint.render(CamelCase.noDelimiter), params)
          val (embeddedData, writer) = params.fieldsLayout.dive(data, key, typeHint.render(params.fnc), params)
          writeObj(embeddedObj, embeddedData, params + writer)
        }
      }
    }
    data
  }
}
