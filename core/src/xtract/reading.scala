package xtract

import java.lang.reflect.Modifier

import scala.reflect.ClassTag

object read {

  type FieldName = List[String]
  type Path = List[FieldName]

  def apply[E](implicit classTag: ClassTag[E]) = new {
    def from[T](data: T)(implicit params: ReadParams[T] = DefaultReadParams): E = {
      readimpl[E, T](classTag.runtimeClass.asInstanceOf[Class[E]], data, params)
    }
  }

  def readimpl[E, T](klass: Class[E], data: T, params: ReadParams[T]): E = {
    val instantiator = {
      val className = klass.getName

      val endsWithNumberRegex = """^(.+)(\d+)$""".r

      val companionObjectClassName = className match {
        case endsWithNumberRegex(x, y) => x + (y.toInt + 1) + "$"
        case _ => className + "$"
      }

      val companionObjectClass = ClassUtils.forName(companionObjectClassName) match {
        case Some(x) => x
        case None => ??? /*uncomment this*/
        /*instantiationException(
            s"$className seems not to be a case class cause we couldn't find it's companion object class $companionObjectClassName"
          )*/
      }

      val companionObject = try {
        val field = companionObjectClass.getField("MODULE$")
        field.get(null)
      } catch {
        case e: NoSuchFieldException => throw new NotCompanionObjectException(companionObjectClass, "it has no MODULE$ field")
      }

      CompanionObjectInstantiator(companionObject)
    }

    val argTypes = instantiator.applyMethod.getParameterTypes.toSeq

    val allFields = klass.getDeclaredFields

    val caseClassFields = allFields.filter(ClassUtils.isPrivate)

    val reader = params.reader

    if (caseClassFields.length == argTypes.length) {
      val args = caseClassFields zip argTypes map { case (field, fieldType) =>
        val fieldName = Utils.splitFieldNameIntoParts(field.getName)
        fieldType match {
          // embedded
          case _ if ClassUtils.isCaseClass(fieldType) => {
            val key = params.layout.makeKey(fieldName, None, params.fnc)
            params.layout.dive(data, key, params) match {
              case Some(Right((data, layout))) => {
                if (reader.accepts(data.getClass)) {
                  readimpl(fieldType, data, params + layout)
                } else {
                  badFieldValue(klass, key, fieldType, data, data.getClass, None)
                }
              }
              case None => throw new MissingFieldException(klass, key, data)
            }
          }
          // embedded polymorphic
          case _ if ClassUtils.isAbstract(fieldType) && !fieldType.isPrimitive => {
            val typeHint = params.thl.get(fieldName, data, params)
            typeHint match {
              case Some(Right(typeHint)) => {
                val tpe = params.thns.guessType(fieldType, typeHint)
                tpe match {
                  case Some(klass) => {
                    val key = params.layout.makeKey(fieldName, Some(typeHint), params.fnc)
                    params.layout.dive(data, key, params) match {
                      case Some(Right((data, layout))) => {
                        if (reader.accepts(data.getClass)) {
                          readimpl(klass, data, params + layout)
                        } else {
                          ??? //badFieldValue(klass, key, fieldType, data, data.getClass, None)
                        }
                      }
                      case None => throw new MissingFieldException(klass, key, data)
                    }
                  }
                  case None => throw new ReadException("bad type hint: " + typeHint)
                }
              }
              case Some(Left(x)) => ??? //badFieldValue(klass, key + "#typehint", classOf[String], x, x.getClass, None)
              case None => throw MissingFieldException(klass, "#typehint", data)
            }
          }
          case _ => {
            val key = params.layout.makeKey(fieldName, None, params.fnc)
            reader.get(data, key) match {
              case Some(value) => {
                val valueType = value.getClass
                fieldType match {
                  // simple
                  case _ if ClassUtils.toNotPrimitive(fieldType).isAssignableFrom(valueType) => value
                  // converters
                  case _ => {
                    val converterOpt = params.converters find { x =>
                      x.canConvertFrom(valueType) && x.canConvertTo(fieldType)
                    }
                    converterOpt match {
                      case Some(converter) => {
                        val option = converter.convert(value, fieldType)
                        option match {
                          case Some(convertedValue) => convertedValue
                          case None => {
                            badFieldValue(klass, key, fieldType, value, valueType, Some(converter))
                          }
                        }
                      }
                      case None => badFieldValue(klass, key, fieldType, value, valueType, None)
                    }
                  }
                }
              }
              case None => throw new MissingFieldException(klass, key, data)
            }
          }
        }
      }
      val castArgs = args.asInstanceOf[Array[Object]]
      val instance = instantiator.applyMethod.invoke(instantiator.companionObject, castArgs: _*)
      instance.asInstanceOf[E]
    } else {
      throw new RuntimeException(s"fields: ${caseClassFields.length}, ctor arity: ${argTypes.length}")
    }
  }
}
