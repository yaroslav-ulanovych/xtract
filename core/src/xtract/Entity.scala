package xtract

import scala.collection.mutable.ArrayBuffer
import scala.language.{existentials, postfixOps}
import scala.reflect.ClassTag



abstract sealed class Entity extends Object with Embeddable with Shortcuts {

  val fields = ArrayBuffer[Field[_]]()

  abstract sealed class Field[T: ClassTag] {
    val classTag = implicitly[ClassTag[T]]
    private var value: Option[T] = None

    var _name: List[String] = _

    def getName(): List[String] = {
      if (_name eq null) {
        val klass = entity.getClass
        val entityClassFields = FirstNextCollection[Class[_]](klass, _.getSuperclass).map(_.getDeclaredFields).flatten
        entityClassFields.find(x => {
          x.setAccessible(true)
          x.get(entity) eq this
        }) match {
          case Some(entityClassField) => setName(entityClassField.getName)
          case None => {
            fields
              .filter(_.isInstanceOf[OptionalField[_]])
              .find(_.asInstanceOf[OptionalField[_]].wrappee eq this) match {
              case Some(optField) => {
                setName(optField.getName())
              }
              case None => {
                throw new IllegalStateException("unable to find field's name")
              }
            }
          }
        }
      }
      _name
    }

    def setName(name: String) {
      setName(name.split("(?=\\p{Upper})").map(_.toLowerCase).toList)
    }

    def setName(name: List[String]) {
      if (_name ne null)
        throw new IllegalStateException("field already has a name " + _name.mkString(""))
      _name = name
    }

    /** dsl */
    def rename(name: String): this.type = {setName(name); this }

    /** An entity the field belongs to. */
    val entity = Entity.this

    /** A class of values the field can contain. */
    val valueClass = classTag.runtimeClass.asInstanceOf[Class[T]]

    fields += this

    /** Getter. */
    def apply(): T = {
      value match {
        case Some(value) => value
        case None => throw new IllegalStateException(s"reading not set field ${Entity.this.getClass.getSimpleName}.${getName()}")
      }
    }

    /** Setter. */
    def := (x: T) {
      value = Some(x)
    }

    def isSet = value.isDefined

    def clear() {
      value = None
    }

    /** Qualified name. */
    def qname: List[String] = entity match {
      case entity: Obj => getName()
      case entity: AbstractObj => /*entity.typeDiscriminator.toLowerCase :: */getName()
    }

    /** Fully qualified name of the field. */
    def fqname: List[List[String]] = (mpath :+ this).map(_.qname)

    def mpath: List[Entity#EmbeddedField[_]] = entity.holder match {
      case Some(field) => field.mpath :+ field
      case None => List()
    }

    override def toString: String = s"""${className}.${qname}"""
  }

  def className = getClass.getSimpleName

  abstract sealed class AbstractSimpleField[T: ClassTag] extends Field[T]

  final class SimpleField[T: ClassTag] extends AbstractSimpleField[T] {
    def this(dflt: T) {
      this()
      this := dflt
    }
  }

  abstract sealed class EmbeddedField[T <: Entity: ClassTag] extends Field[T] {
    override def :=(x: T) {
      x.wasEmbeddedInto(this)
      super.:=(x)
    }

  }

  final class EmbeddedConcreteField[T <: Obj: ClassTag] extends EmbeddedField[T] {
  }

  final class EmbeddedPolymorphicField[T <: AbstractObj: ClassTag] extends EmbeddedField[T]

  /**
   * Why to introduce a separate class for links if we can use plain fields?
   * ODocument.data("entityId") returns not an id but a whole entity document,
   * which is not compatible with the id type of course.
   */
  final class LinkField[T: ClassTag] extends AbstractSimpleField[T]

  final class CustomField[CustomType: ClassTag, BackingType: ClassTag](
    val serialize: CustomType => BackingType,
    val deserialize: BackingType => Option[CustomType]
  )
    extends AbstractSimpleField[CustomType]
  {
    val backingClassTag = implicitly[ClassTag[BackingType]]
    val backingClass = backingClassTag.runtimeClass
  }

  /**
   * Phantom field is a special field that is not present in data an entity is reading from.
   * Instead a user provided function is used to retrieve the value of the field.
   * That's handy when a part of the entity information is located in the context.
   */
  //todo shouldn't have a name?
  final class PhantomField[T: ClassTag](val deserialize: () => Option[T]) extends Field[T]

  final class OptionalField[T: ClassTag](val wrappee: Field[T]) extends Field[Option[T]] {
    fields -= wrappee
  }

  def write[To](params: WriteParams[To] = DefaultWriteParams): To = {
    writeimpl(params.writer.create, params)
  }

  def writeimpl[To](data: To, params: WriteParams[To]): To = {
    ???
//    if (this.isInstanceOf[AbstractObj]) {
//      val entity = this.asInstanceOf[AbstractObj]
//      val typeHint = params.thns.getTypeHint(entity)
//      val key = params.layout.makeKey(List(), Some(typeHint), params.fnc)
//      val (data2, _) = params.layout.dive(data, key, params)
//    }
//    fields.foreach(field => {
//      field match {
//        case field: SimpleField[_] => {
//          val key = params.layout.makeKey(field.getName(), None, params.fnc)
//          params.writer.put(data, key, field())
//        }
//        case field_ : CustomField[_, _] => {
//          val field = field_.asInstanceOf[CustomField[Any, Any]]
//          val key = params.layout.makeKey(field.getName(), None, params.fnc)
//          params.writer.put(data, key, field.serialize(field()))
//        }
//        case field_ : EmbeddedConcreteField[_] => {
//          val field = field_.asInstanceOf[EmbeddedConcreteField[Obj]]
//          val key = params.layout.makeKey(field.getName(), None, params.fnc)
//          val (data2, layout) = params.layout.dive(data, key, params)
//          val entity = field()
//          entity.writeimpl(data2, params + layout)
//        }
//        case field_ : EmbeddedPolymorphicField[_] => {
//          val field = field_.asInstanceOf[EmbeddedPolymorphicField[AbstractObj]]
//          val entity = field()
//          val typeHint = params.thns.getTypeHint(entity)
//          val key = params.layout.makeKey(field.getName(), Some(typeHint), params.fnc)
//          val (data2, layout) = params.layout.dive(data, key, params)
//          val newParams = params + layout
//          params.writer.put(data2, params.layout.makeKey(List("type"), None, params.fnc), typeHint)
//          entity.writeimpl(data2, newParams)
//        }
//        case field: LinkField[_] => {
//          //          params.put(data, field, field())
//        }
//      }
//    })
//
//    data
  }
}


abstract class Obj extends Entity

abstract class AbstractObj extends Entity {
  def abstractClass: Class[_] = {
    val klass = getClass
    val interfaces = klass.getInterfaces
    val superClass = klass.getSuperclass

    // crutches
    if (superClass == classOf[AbstractObj] && interfaces.length == 1) {
      interfaces(0)
    } else if (superClass.getSuperclass == classOf[AbstractObj]) {
      superClass
    } else {
      throw new Exception("unable to find abstract class name for " + klass.getName)
    }
  }


  def abstractClassName: String = abstractClass.getSimpleName

  //todo move to reader
  def typeDiscriminator: String = {
    val x = abstractClassName
    val y = getClass.getSimpleName
    if (y.endsWith(x)) {
      y.substring(0, y.length - x.length)
    } else {
      y
    }
  }
}

trait Embeddable {
  self: Entity =>

  private var _fieldThatThisEntityWasEmbeddedInto: Entity#EmbeddedField[Entity] = _

  def holder(): Option[Entity#EmbeddedField[Entity]] = {
    Option(_fieldThatThisEntityWasEmbeddedInto)
  }

  def wasEmbeddedInto[T <: Entity](field: Entity#EmbeddedField[T]) {
    _fieldThatThisEntityWasEmbeddedInto = field.asInstanceOf[Entity#EmbeddedField[Entity]]
  }
}

trait Shortcuts {
  self: Entity =>

  def int = new SimpleField[Int]
  def long = new SimpleField[Long]
  def float = new SimpleField[Float]
  def double = new SimpleField[Double]
  def string = new SimpleField[String]
  def bool = new SimpleField[Boolean]
  def field[T <: Any: ClassTag] = new SimpleField[T]
  def field[T <: Any: ClassTag](dflt: T) = new SimpleField[T](dflt)
  def link[T <: Entity with Id](implicit ClassTag: ClassTag[T#Id]) = new LinkField[T#Id]
  def embedded[T <: Obj](implicit dummy: DummyImplicit, classTag: ClassTag[T]) = new EmbeddedConcreteField[T]
  def embedded[T <: AbstractObj: ClassTag] = new EmbeddedPolymorphicField[T]
  def custom[T: ClassTag, U: ClassTag](serialize: T => U, deserialize: U => Option[T]) = new CustomField[T, U](serialize, deserialize)
  def optional[T: ClassTag](field: Field[T]) = new OptionalField(field)
}

trait FieldTypeShortcuts {
  type Field = Entity#Field[_]
  type EmbeddedField = Entity#EmbeddedField[_]
  type EmbeddedConcreteField = Entity#EmbeddedConcreteField[_]
  type EmbeddedPolymorphicField = Entity#EmbeddedPolymorphicField[AbstractObj]
}

object Obj {
  def create[T <: Entity: ClassTag](f: T => Unit): T = {
    val x = implicitly[ClassTag[T]].runtimeClass.newInstance().asInstanceOf[T]
    f(x)
    x
  }
}

trait Id {
  self: Entity =>
  type Id
  def id: Field[Id]
}

case class FieldName(xs: List[String])
case class Path(xs: List[FieldName])