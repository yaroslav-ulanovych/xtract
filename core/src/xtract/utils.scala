package xtract

import java.lang.reflect.{Field, Modifier}

case class FirstNextCollection[T <: AnyRef](first: T, next: (T) => T) extends Traversable[T] {
  def foreach[U](f: (T) => U): Unit = {
    var x = first
    while (x ne null) {
      f(x)
      x = next(x)
    }
  }
}

object Utils {
  def splitFieldNameIntoParts(x: String): List[String] = x.split("""(?=\p{Lu})""").toList
}

object ClassUtils {
  def forName[T](name: String): Option[Class[T]] = {
    try {
      val klass = Class.forName(name)
      Some(klass.asInstanceOf[Class[T]])
    } catch {
      case _: ClassNotFoundException => None
    }
  }

  def forName[T](pkg: Package, name: String, suffix: String): Option[Class[T]] = {
    val fqname = pkg.getName + "." + name + suffix
    forName(fqname)
  }

  // naive check
  def isCaseClass(klass: Class[_]): Boolean = classOf[Product].isAssignableFrom(klass)

  def isAbstract(klass: Class[_]) = Modifier.isAbstract(klass.getModifiers)

  def isPrivate(field: Field) = Modifier.isPrivate(field.getModifiers)

  def toNotPrimitive(klass: Class[_]) = klass.getName match {
    case "int" => classOf[Integer]
    case x => klass
  }

}


object StringUtils {
  def parseInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case _: NumberFormatException => None
    }
  }

  def capitalizeFirstLetter(s: String): String = {
    val chars = s.toCharArray
    chars(0) = Character.toUpperCase(chars(0))
    new String(chars)
  }
}

class NotSupportedOperationException(description: String) extends Exception(description)
class UnreachableOperationException(reason: String) extends Exception(reason)

object unreachable {
  def apply() = throw new UnreachableOperationException("unknown reason")
}