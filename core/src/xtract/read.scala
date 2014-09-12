package xtract

import java.lang.reflect.Modifier

import scala.reflect.ClassTag

object read {

  type FieldName = List[String]
  type Path = List[FieldName]

  def apply[T](implicit classTag: ClassTag[T]) = new {
    def from[U](data: U)(implicit params: ReadParams[U] = DefaultReadParams): T = {
      read1(classTag.runtimeClass.asInstanceOf[Class[T]], data, params)
    }
  }

  def read1[T, U](klass: Class[T], data: U, params: ReadParams[U]): T = {
    if (ClassUtils.isAbstract(klass)) {
      getTypeHint(data, params) match {
        case Some(Right(typeHint)) => {
          params.thns.guessType(klass, typeHint) match {
            case Some(klass) => {
              params.layout.dive(data, params.fnc.apply(List(typeHint)), params) match {
                case Some(Right((data, layout))) => {
                  read2(klass.asInstanceOf[Class[T]], data, params + layout)
                }
                case Some(Left(x)) => ???
                case None => ???
              }

            }
            case None => ???
          }
        }
        case Some(Left(x)) => ???
        case None => ???
      }
    } else {
      read2(klass, data, params)
    }
  }

  def read2[T, U](klass: Class[T], data: U, params: ReadParams[U]): T = {
    if (classOf[Entity].isAssignableFrom(klass)) {
      val entity = klass.newInstance().asInstanceOf[Entity]
      reado(entity.fields, data, params)
      entity.asInstanceOf[T]
    } else {
      readcc(klass, data, params)
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
          case _ if ClassUtils.isCaseClass(fieldType) => {
            val key = params.layout.makeKey(fieldName, None, params.fnc)
            params.layout.dive(data, key, params) match {
              case Some(Right((data, layout))) => {
                if (reader.accepts(data.getClass)) {
                  readcc(fieldType, data, params + layout)
                } else {
                  ??? //badFieldValue(klass, key, fieldType, data, data.getClass, None)
                }
              }
              case None => throw new MissingFieldException(klass, key, data)
            }
          }
          // embedded polymorphic
          case _ if ClassUtils.isAbstract(fieldType) && !fieldType.isPrimitive => {
            val typeHint = getTypeHint(data, params)
            typeHint match {
              case Some(Right(typeHint)) => {
                val tpe = params.thns.guessType(fieldType, typeHint)
                tpe match {
                  case Some(klass) => {
                    val key = params.layout.makeKey(fieldName, Some(typeHint), params.fnc)
                    params.layout.dive(data, key, params) match {
                      case Some(Right((data, layout))) => {
                        if (reader.accepts(data.getClass)) {
                          readcc(klass, data, params + layout)
                        } else {
                          ??? //badFieldValue(klass, key, fieldType, data, data.getClass, None)
                        }
                      }
                      case None => throw new MissingFieldException(klass, key, data)
                    }
                  }
                  case None => ??? //throw new ReadException("bad type hint: " + typeHint)
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

  def reado[Data](fields: Traversable[Entity#Field[_]], data: Data, params: ReadParams[Data]) {
    for(field <- fields) {
      field match {
        case field_ : Entity#SimpleField[_] => {
          val field = field_.asInstanceOf[Entity#Field[Any]]
          val key = params.layout.makeKey(field.getName(), None, params.fnc)
          params.reader.get(data, key) match {
            case Some(v) => {
              if (field.valueClass.isAssignableFrom(v.getClass)) {
                field := v
              } else {
                params.converters.find(x => x.canConvertFrom(v.getClass) && x.canConvertTo(field.valueClass)) match {
                  case Some(converter) => {
                    converter.convert(v, field.valueClass) match {
                      case Some(value) => field := value
                      case None => ??? //throw BadFieldValueException(rootObj.getClass, field, v, key, data, Some(converter))
                    }
                  }
                  case None => throw BadFieldValueException(field.entity.getClass, key, field.valueClass, v, v.getClass, None)
                }
              }
            }
            case None => throw new MissingFieldException(field.entity.getClass, key, data)
          }
        }
        case field_ : Entity#CustomField[_, _] => {
          val field = field_.asInstanceOf[Entity#CustomField[Any, Any]]
          val key = params.layout.makeKey(field.getName(), None, params.fnc)
          params.reader.get(data, key) match {
            case Some(value) => {
              if (field.backingClass.isAssignableFrom(value.getClass)) {
                field.deserialize(value) match {
                  case Some(value) => {
                    field := value
                  }
                  case None => ??? //throw new BadFieldValueException(rootObj, field, value, key, data, None)
                }
              } else {
                ??? //throw new BadFieldValueException(rootObj, field, value, key, data, None)
              }
            }
            case None => ??? //throw new MissingFieldException(rootObj, key, data, field)
          }
        }
        case field_ : Entity#EmbeddedConcreteField[_] => {
          val field = field_.asInstanceOf[Entity#EmbeddedConcreteField[Obj]]
          val key = params.layout.makeKey(field.getName(), None, params.fnc)
          params.layout.dive(data, key, params) match {
            case Some(Right((data, layout))) => {
              val entity = field.valueClass.newInstance()
              reado(entity.fields, data, params + layout)
              field := entity
            }
            case Some(Left(v)) => ???
            case None => ???
          }
        }
        case field_ : Entity#EmbeddedPolymorphicField[_]  => {
          val field = field_.asInstanceOf[Entity#EmbeddedPolymorphicField[AbstractObj]]
          val typeHint = xtract.read.getTypeHint(data, params) match {
            case Some(Right(x)) => x
            case Some(Left(x)) => ???
            case None => throw MissingTypeHintException(field.entity.getClass, field.getName().mkString("_"), field.valueClass, data)
          }
          params.thns.guessType(field.classTag.runtimeClass, typeHint) match {
            case Some(klass) => {
              val key = params.layout.makeKey(field.getName(), Some(typeHint), params.fnc)
              params.layout.dive(data, key, params) match {
                case Some(Right((data, layout))) => {
                  val entity = klass.newInstance().asInstanceOf[AbstractObj]
                  reado(entity.fields, data, params + layout)
                  field := entity
                }
                case Some(Left(v)) => ???
                case None => ??? //throw new MissingFieldException(rootObj, key, data, field)
              }
            }
            case None => ???
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


  def getTypeHint[T](data: T, params: ReadParams[T]): Option[Either[Any, String]] = {
    params.reader.get(data, params.layout.makeKey(List("type"), None, params.fnc)) match {
      case Some(x: String) => Some(Right(x))
      case Some(x) => Some(Left(x))
      case None => None
    }
  }
}
