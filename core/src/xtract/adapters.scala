package xtract

import java.util.{Map => JavaMap}

import scala.collection.GenMap
import scala.reflect.ClassTag

// def begin Adapter
abstract class Reader[-T: ClassTag] {
  val classTag: ClassTag[_] = implicitly[ClassTag[T]]
  def accepts(klass: Class[_]): Boolean = implicitly[ClassTag[T]].runtimeClass.isAssignableFrom(klass)
  def get_!(data: T, key: String): Any = get(data, key).getOrElse(throw new MissingKeyException(data, key))

  def get(data: T, key: String): Option[Any]
}
// def end

// def begin MapAdapter
object MapReader extends Reader[GenMap[String, Any]] {
  def get(data: GenMap[String, Any], key: String): Option[Any] = data.get(key)
}
// def end

// def begin JavaMapAdapter
object JavaMapReader extends Reader[JavaMap[String, Any]] {
  def get(data: JavaMap[String, Any], key: String): Option[Any] = Option(data.get(key))
}
// def end

case class PrefixedReader[T](prefix: String, reader: Reader[T]) extends Reader[T]()(reader.classTag.asInstanceOf[ClassTag[T]]/*can't make ClassTag[T] because of variance issues*/) {
  def get(data: T, key: String): Option[Any] = reader.get(data, prefix + key)

  override def get_!(data: T, key: String): Any = {
    try {
      super.get_!(data, key)
    } catch {
      case MissingKeyException(_, key) => throw MissingKeyException(data, prefix + key)
    }
  }
}