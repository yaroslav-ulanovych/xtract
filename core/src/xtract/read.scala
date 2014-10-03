package xtract

import java.lang.reflect.{ParameterizedType, Modifier}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

object read extends Object with FieldTypeShortcuts {

  def apply[T](implicit classTag: ClassTag[T]) = new {
    def from[U](data: U)(implicit params: ReadParams[U] = DefaultReadParams): T = {
      read1(classTag.runtimeClass.asInstanceOf[Class[T]], data, params)
    }
  }

  def read1[T, U](klass: Class[T], data: U, params: ReadParams[U]): T = {
    if (ClassUtils.isAbstract(klass)) {
      val typeHint = getTypeHint(data, params) match {
        case Right(Right(typeHint)) => typeHint
        case Right(Left(x)) => ???
        case Left => ???
      }
      params.thns.guessType(klass, typeHint) match {
        case Some(klass) => {
          params.layout.dive2(data, params.fnc.apply(List(typeHint)), params) match {
            case Some(Right((data, layout))) => {
              read2(klass.asInstanceOf[Class[T]], data, params + layout)
            }
            case Some(Left(x)) => ???
            case None => ???
          }

        }
        case None => ???
      }
    } else {
      read2(klass, data, params)
    }
  }

  def read2[T, U](klass: Class[T], data: U, params: ReadParams[U]): T = {
    klass match {
      case _ if classOf[Entity].isAssignableFrom(klass) => {
        val entity = klass.newInstance().asInstanceOf[Entity]
        reado(entity.fields, data, params)
        entity.asInstanceOf[T]
      }
      case _ if ClassUtils.isCaseClass(klass) => {
        readcc(klass, data, params)
      }
      case _ if klass.isAssignableFrom(data.getClass) => {
        data.asInstanceOf[T]
      }
      case _ => {
        params.converters.find(x => x.canConvertFrom(data.getClass) && x.canConvertTo(klass)) match {
          case Some(converter) => {
            converter.asInstanceOf[Converter[Any, Any]].convert(data, klass) match {
              case Some(value) => value.asInstanceOf[T]
              case None => throw new Exception(s"can't read ${klass.getName} from ${data.getClass.getName}($data) with converter $converter")
            }
          }
          case None => throw new Exception(s"can't read ${klass.getName} from ${data.getClass.getName}($data)")
        }
      }
    }
  }

  def readCollection(collectionClass: Class[_], itemClass: Class[_], xs: Traversable[_], params: ReadParams[_]): Any = {
    val buffer = ArrayBuffer[Any]()
    xs foreach { x =>
      buffer += read1(itemClass, x, params.asInstanceOf[ReadParams[Any]])
    }
    collectionClass match {
      case x if x == classOf[List[_]] => buffer.toList
      case x if x == classOf[Seq[_]] => buffer.toSeq
      case other => throw new Exception(s"${collectionClass.getName} isn't supported")
    }
  }

  private def readcc[E, T](klass: Class[E], data: T, params: ReadParams[T]): E = {
    val className = klass.getName

    val endsWithNumberRegex = """^(.+)(\d+)$""".r

    val companionObjectClassName = className match {
      case endsWithNumberRegex(x, y) => x + (y.toInt + 1) + "$"
      case _ => className + "$"
    }

    val companionObjectClass = ClassUtils.forName(companionObjectClassName) match {
      case Some(x) => x
      case None => {
        val msg = s"$className seems not to be a case class cause we couldn't find it's companion object class $companionObjectClassName"
        throw new Exception(msg)
      }
    }

    val companionObject = try {
      val field = companionObjectClass.getField("MODULE$")
      field.get(null)
    } catch {
      case e: NoSuchFieldException => throw new NotCompanionObjectException(companionObjectClass, "it has no MODULE$ field")
    }

    val instantiator = CompanionObjectInstantiator(companionObject)

    val argTypes = instantiator.applyMethod.getParameterTypes.toSeq

    val allFields = klass.getDeclaredFields

    val caseClassFields = allFields.filter(ClassUtils.isPrivate)

    val reader = params.reader

    if (caseClassFields.length == argTypes.length) {
      val args = caseClassFields zip argTypes map { case (field, fieldType) =>
        val fieldName = Utils.splitFieldNameIntoParts(field.getName)
        fieldType match {
          // embedded
//          case _ if ClassUtils.isCaseClass(fieldType) => {
//            val key = params.layout.makeKey(fieldName, None, params.fnc)
//            params.layout.dive(data, key, params) match {
//              case Some(Right((data, layout))) => {
//                if (reader.accepts(data.getClass)) {
//                  readcc(fieldType, data, params + layout)
//                } else {
//                  ??? //badFieldValue(klass, key, fieldType, data, data.getClass, None)
//                }
//              }
//              case None => throw new MissingFieldException(klass, key, data)
//            }
//          }
          // embedded polymorphic
//          case _ if ClassUtils.isAbstract(fieldType) && !fieldType.isPrimitive => {
//            val typeHint = getTypeHint(data, params)
//            typeHint match {
//              case Right(Right(typeHint)) => {
//                val tpe = params.thns.guessType(fieldType, typeHint)
//                tpe match {
//                  case Some(klass) => {
//                    val key = params.diver.makeKey(fieldName, Some(typeHint), params.fnc)
//                    params.layout.dive(data, key, params) match {
//                      case Some(Right((data, layout))) => {
//                        if (reader.accepts(data.getClass)) {
//                          readcc(klass, data, params + layout)
//                        } else {
//                          ??? //badFieldValue(klass, key, fieldType, data, data.getClass, None)
//                        }
//                      }
//                      case None => throw new MissingFieldException(klass, key, data)
//                    }
//                  }
//                  case None => ??? //throw new ReadException("bad type hint: " + typeHint)
//                }
//              }
//              case Right(Left(x)) => ??? //badFieldValue(klass, key + "#typehint", classOf[String], x, x.getClass, None)
//              case Left => throw MissingFieldException(klass, "#typehint", data)
//            }
//          }
          case _ => {
            val key = params.layout.makeKey(fieldName, params.fnc)
            reader.get(data, key) match {
              case Some(value) => {
                val valueType = value.getClass
                fieldType match {
                  // collection
                  case _ if isCollection(fieldType) => {
                    val collectionClass = fieldType
                    val itemClass = getTypeArgumentOfParameterizedCaseClassField(klass, field.getName)
                    value match {
                      case collection: Traversable[_] => {
                        readCollection(collectionClass, itemClass, collection, params)
                      }
                      case value => ???
                    }
                  }
                  // simple
                  case _ if ClassUtils.toNotPrimitive(fieldType).isAssignableFrom(valueType) => value
                  // converters
                  case _ => {
                    val converterOpt = params.converters find { x =>
                      x.canConvertFrom(valueType) && x.canConvertTo(fieldType)
                    }
                    converterOpt match {
                      case Some(converter) => {
                        val option = converter.asInstanceOf[Converter[Any, Any]].convert(value, fieldType)
                        option match {
                          case Some(convertedValue) => convertedValue
                          case None => {
                            ??? //badFieldValue(klass, key, fieldType, value, valueType, Some(converter))
                          }
                        }
                      }
                      case None => ??? //badFieldValue(klass, key, fieldType, value, valueType, None)
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

  def isAssignable(dst: Class[_], src: Class[_]): Boolean = {
    if (dst.isPrimitive) {
      if (dst == classOf[Int] && src == classOf[java.lang.Integer]) return true
      if (dst == classOf[Long] && src == classOf[java.lang.Long]) return true
      if (dst == classOf[Float] && src == classOf[java.lang.Float]) return true
      if (dst == classOf[Double] && src == classOf[java.lang.Double]) return true
      if (dst == classOf[Boolean] && src == classOf[java.lang.Boolean]) return true
      return false
    } else {
      dst.isAssignableFrom(src)
    }
  }

  def reado[Data](fields: Traversable[Entity#Field[_]], data: Data, params: ReadParams[Data]) {
    for(field <- fields) {
      field match {
        case field_ : SimpleField => {
          val field = field_.asInstanceOf[Entity#Field[Any]]
          val key = params.layout.makeKey(field.getName().words, params.fnc)
          params.reader.get(data, key) match {
            case Some(v) => {
              if (isAssignable(field.valueClass, v.getClass)) {
                field := v
              } else {
                params.converters.find(x => x.canConvertFrom(v.getClass) && x.canConvertTo(field.valueClass)) match {
                  case Some(converter) => {
                    converter.asInstanceOf[Converter[Any, Any]].convert(v, field.valueClass) match {
                      case Some(value) => field := value
                      case None => throw BadFieldValueException(field.entity.getClass, key, field.valueClass, v, v.getClass, Some(converter))
                    }
                  }
                  case None => throw BadFieldValueException(field.entity.getClass, key, field.valueClass, v, v.getClass, None)
                }
              }
            }
            case None => throw new MissingFieldException(field.entity.getClass, key, data)
          }
        }
//        case field_ : Entity#CustomField[_, _] => {
//          val field = field_.asInstanceOf[Entity#CustomField[Any, Any]]
//          val key = params.diver.makeKey(field.getName(), None, params.fnc)
//          params.reader.get(data, key) match {
//            case Some(value) => {
//              if (field.backingClass.isAssignableFrom(value.getClass)) {
//                field.deserialize(value) match {
//                  case Some(value) => {
//                    field := value
//                  }
//                  case None => ??? //throw new BadFieldValueException(rootObj, field, value, key, data, None)
//                }
//              } else {
//                ??? //throw new BadFieldValueException(rootObj, field, value, key, data, None)
//              }
//            }
//            case None => ??? //throw new MissingFieldException(rootObj, key, data, field)
//          }
//        }
//        case field_ : Entity#EmbeddedConcreteField[_] => {
//          val field = field_.asInstanceOf[Entity#EmbeddedConcreteField[Obj]]
//          val key = params.diver.makeKey(field.getName(), None, params.fnc)
//          params.layout.dive(data, key, params) match {
//            case Some(Right((data, layout))) => {
//              val entity = field.valueClass.newInstance()
//              reado(entity.fields, data, params + layout)
//              field := entity
//            }
//            case Some(Left(v)) => ???
//            case None => ???
//          }
//        }
        case field_ : Entity#EmbeddedPolymorphicField[_]  => {
          val field = field_.asInstanceOf[Entity#EmbeddedPolymorphicField[AbstractObj]]
          params.layout.dive1(data, params.fnc.apply(field.getName().words), params) match {
            case Some(Right((data, layout))) => {
              field := read1(field.valueClass, data, params + layout)
            }
            case Some(Left(v)) => ???
            case None => throw MissingTypeHintException(field.entity.getClass, field.getName().words.mkString, field.valueClass, data)
          }
        }
        //        case field: LinkField[_] => {
        //          field.asInstanceOf[LinkField[Any]] := params.get(field, data)
        //        }
        //        case field: PhantomField[_] => {
        //          field.deserialize() match {
        //            case Some(x) => field := x
        //            case None => throw new MissingFieldException(field, null, params)
        //          }
        //        }
        //        case field: OptionalField[_] => {
        //          val v = try {
        //            read(List(field.wrappee), data, params)
        //            Some(field.wrappee())
        //          } catch {
        //            case e: MissingFieldException if (e.field == field.wrappee) => None
        //            case e: MissingTypeHintException => None
        //          }
        //          field := v
        //        }
      }
    }
  }


  def getTypeHint[T](data: T, params: ReadParams[T]): Either[String, Either[Any, String]] = {
    val key = params.layout.makeKey(List("type"), params.fnc)
    params.reader.get(data, key) match {
      case Some(x: String) => Right(Right(x))
      case Some(x) => Right(Left(x))
      case None => Left(key)
    }
  }

  def isCollection(klass: Class[_]): Boolean = classOf[Traversable[_]].isAssignableFrom(klass)


  def getTypeArgumentOfParameterizedCaseClassField(klass: Class[_], field: String): Class[_] = {
    import scala.util.control.Exception._
    catching(classOf[NoSuchMethodException]).opt(klass.getMethod(field)) match {
      case Some(method) => {
        method.getGenericReturnType match {
          case tpe: ParameterizedType => {
            tpe.getActualTypeArguments match {
              case Array(tpe) => {
                tpe match {
                  case klass: Class[_] => klass
                  case _ => ???
                }
              }
              case _ => ???
            }
          }
          case _ => ???
        }
      }
      case None => ???
    }
  }


}
