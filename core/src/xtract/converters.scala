package xtract

import scala.reflect.ClassTag


trait Converter[From, To] {
  def canConvertFrom(klass: Class[_]): Boolean
  def canConvertTo(klass: Class[_]): Boolean
  def convert(value: From, expected: Class[_ <: To]): Option[To]
  def convertBack(value: To): From
}



class SimpleConverter[From: ClassTag, To: ClassTag](f: From => Option[To], g: To => From) extends Converter[From, To] {
  val srcClass = implicitly[ClassTag[From]].runtimeClass
  val dstClass = implicitly[ClassTag[To]].runtimeClass

  def canConvertFrom(klass: Class[_]) = klass == srcClass
  def canConvertTo(klass: Class[_]) = klass == dstClass

  def convert(value: From, expected: Class[_ <: To]): Option[To] = f(value)

  def convertBack(value: To): From = g(value)

  override def toString = s"Converter[${srcClass.getName}, ${dstClass.getName}]"
}

object Converter {
  def apply[From: ClassTag, To: ClassTag](f: From => Option[To], g: To => From): Converter[From, To] = {
    new SimpleConverter(f, g)
  }
}

object BuiltInConverters {
  object BigIntToInt extends SimpleConverter[BigInt, Int](x => if (x.isValidInt) Some(x.intValue) else None, BigInt.apply)
}


object StringToJavaEnumConverter extends Converter[String, Enum[_]] {
  def canConvertFrom(klass: Class[_]) = klass == classOf[String]

  def canConvertTo(klass: Class[_]) = klass.isEnum

  def convert(value: String, expected: Class[_ <: Enum[_]]): Option[Enum[_]] = {
    expected.getEnumConstants().find(_.name() == value)
  }

  def convertBack(value: Enum[_]): String = value.name()
}