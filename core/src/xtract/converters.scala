package xtract

import scala.reflect.ClassTag


// def begin Converter
trait Converter {
  def canConvertFrom(klass: Class[_]): Boolean
  def canConvertTo(klass: Class[_]): Boolean
  def convert(value: Any, expected: Class[_]): Option[Any]
}
// def end

class SimpleConverter[From: ClassTag, To: ClassTag](f: From => Option[To]) extends Converter {
  val srcClass = implicitly[ClassTag[From]].runtimeClass
  val dstClass = implicitly[ClassTag[To]].runtimeClass

  def canConvertFrom(klass: Class[_]) = klass == srcClass
  def canConvertTo(klass: Class[_]) = klass == dstClass

  def convert(value: Any, expected: Class[_]): Option[Any] = {
    f(value.asInstanceOf[From])
  }

  override def toString = s"Converter[${srcClass.getName}, ${dstClass.getName}]"
}

object Converter {
  def apply[From: ClassTag, To: ClassTag](f: From => Option[To]): Converter = {
    new SimpleConverter(f)
  }
}

object BuiltInConverters {
  object BigIntToInt extends SimpleConverter((f: BigInt) => if (f.isValidInt) Some(f.intValue) else None)
  object IntegerToInt extends SimpleConverter[Integer, Int](x => Some(x))
}

// def begin JavaEnumConverter
object JavaEnumConverter extends Converter {
// It accepts strings or integers
  def canConvertFrom(klass: Class[_]) = {
    klass == classOf[Integer] || klass == classOf[String]
  }
// can convert to any enumeration.
  def canConvertTo(klass: Class[_]) = klass.isEnum
// Well, I don't know what to explain here :)
  def convert(value: Any, expected: Class[_]): Option[Any] = {
    val xs = expected.getEnumConstants()
    value match {
      case i: Integer => {
        if (i >= 0 && i < xs.length) Some(xs(i)) else None
      }
      case s: String => xs.find(_.asInstanceOf[Enum[_]].name() == s)
    }
  }
}
// def end