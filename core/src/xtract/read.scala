package xtract

import java.lang.reflect.{ParameterizedType, Modifier}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

object read extends Object with FieldTypeShortcuts {

  def apply[T](implicit classTag: ClassTag[T]) = new {
    def from[U](data: U)(implicit params: ReadParams[U] = DefaultReadParams): T = {
      readAny(classTag.runtimeClass.asInstanceOf[Class[T]], data, params)
    }
  }

  def readAny[T, U](klass: Class[T], data: U, params: ReadParams[U]): T = {
    klass match {
      case _ if ClassUtils.isAbstract(klass) && !klass.isPrimitive => {
        readPolymorphic(klass, data, params)
      }
      case _ if classOf[Entity].isAssignableFrom(klass) => {
        val entity = klass.newInstance().asInstanceOf[Entity]
        readObject(entity, entity.fields, data, params)
        entity.asInstanceOf[T]
      }
      case _ if ClassUtils.isCaseClass(klass) => {
        readCaseClass(klass, data, params)
      }
      case _ if isAssignable(klass, data.getClass) => data.asInstanceOf[T]

    }
  }

  def readPolymorphic[T, U](klass: Class[T], data: U, params: ReadParams[U]): T = {
    val typeHint = getTypeHint(data, params) match {
      case (Some(Right(typeHint)), _) => typeHint
      case (Some(Left(x)), _) => ???
      case (None, key) => throw new MissingTypeHintException(klass, key, classOf[String], data)
    }
    params.thns.guessType(klass, typeHint) match {
      case Some(klass) => {
        params.layoutOld.dive2(data, params.fnc.apply(List(typeHint)), params) match {
          case Some(Right((data, layout))) => {
            readAny(klass.asInstanceOf[Class[T]], data, params + layout)
          }
          case Some(Left(x)) => ???
          case None => ???
        }

      }
      case None => ???
    }
  }

  def readCollection(collectionClass: Class[_], itemClass: Class[_], xs: Traversable[_], params: ReadParams[_]): Any = {
    val buffer = ArrayBuffer[Any]()
    xs foreach { x =>
      buffer += readAny(itemClass, x, params.asInstanceOf[ReadParams[Any]])
    }
    collectionClass match {
      case x if x == classOf[List[_]] => buffer.toList
      case x if x == classOf[Seq[_]] => buffer.toSeq
      case other => throw new Exception(s"${collectionClass.getName} isn't supported")
    }
  }

  private def readCaseClass[E, T](klass: Class[E], data: T, params: ReadParams[T]): E = {
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
          // polymorphic
          case _ if ClassUtils.isAbstract(fieldType) && !fieldType.isPrimitive => {
            val key = params.layoutOld.makeKey(fieldName, params.fnc)
            params.layoutOld.dive1(data, key, params) match {
              case Some(Right((data, layout))) => {
                readPolymorphic(fieldType, data, params + layout)
              }
              case Some(Left(value)) => throw new BadFieldValueException(klass, key, fieldType, value, value.getClass, None)
              case None => throw new MissingFieldException(klass, key, fieldType, data)
            }
          }
          // embedded case class
          case _ if ClassUtils.isCaseClass(fieldType) => {
            val key = params.fnc.apply(fieldName)
            params.diver.dive(data, key, params) match {
              case Some(Right((data, reader))) => {
                readCaseClass(fieldType, data, params + reader)
              }
              case Some(Left(value)) => {
                throw new BadFieldValueException(klass, field.getName, fieldType, value, value.getClass, None)
              }
              case None => {
                throw new MissingFieldException(klass, field.getName, fieldType, data)
              }
            }
          }
          case _ =>
            readSimpleField(klass, fieldType, fieldName, params, data)
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

  def readSimpleField[T](root: Class[_], fieldType: Class[_], name: List[String], params: ReadParams[T], data: T): Any = {
    val key = params.layoutOld.makeKey(name, params.fnc)
    params.reader.get(data, key) match {
      case Some(value) => {
        if (isAssignable(fieldType, value.getClass)) {
          value
        } else {
          params.converters.find(x => x.canConvertFrom(value.getClass) && x.canConvertTo(fieldType)) match {
            case Some(converter) => {
              converter.asInstanceOf[Converter[Any, Any]].convert(value, fieldType) match {
                case Some(value) => {
                  value
                }
                case None => throw new BadFieldValueException(root, key, fieldType, value, value.getClass, Some(converter))
              }
            }
            case None => throw new BadFieldValueException(root, key, fieldType, value, value.getClass, None)
          }
        }
      }
      case None => throw new MissingFieldException(root, key, fieldType, data)
    }
  }

  def readObject[Data](obj: Entity, fields: Traversable[Entity#Field[_]], data: Data, params: ReadParams[Data]) {
    val klass = obj.getClass
    for(field <- fields) {
      val fieldType = field.valueClass
      field match {
        case fld : SimpleField => {
          val field = fld.asInstanceOf[Entity#Field[Any]]
          val value = readSimpleField(obj.getClass, field.valueClass, field.getName().words, params, data)
          field := value
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
        case field_ : Entity#EmbeddedConcreteField[_] => {
          val field = field_.asInstanceOf[Entity#EmbeddedConcreteField[Obj]]

          val key = params.fnc.apply(field.getName())

          params.diver.dive(data, key, params) match {
            case Some(Right((data, reader))) => {
              val obj = field.valueClass.newInstance()
              readObject(obj, obj.fields, data, params + reader)
              field := obj
            }
            case Some(Left(value)) => {
              throw new BadFieldValueException(klass, key, fieldType, value, value.getClass, None)
            }
            case None => {
              throw new MissingFieldException(klass, key, fieldType, data)
            }
          }
        }
        case field_ : Entity#EmbeddedPolymorphicField[_]  => {
          val field = field_.asInstanceOf[Entity#EmbeddedPolymorphicField[AbstractObj]]
          params.layoutOld.dive1(data, params.fnc.apply(field.getName().words), params) match {
            case Some(Right((data, layout))) => {
              field := readAny(field.valueClass, data, params + layout)
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


  def getTypeHint[T](data: T, params: ReadParams[T]): (Option[Either[Any, String]], String) = {
    val key = params.layoutOld.makeKey(List("type"), params.fnc)
    params.reader.get(data, key) match {
      case Some(x: String) => (Some(Right(x)), key)
      case Some(x) => (Some(Left(x)), key)
      case None => (None, key)
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
