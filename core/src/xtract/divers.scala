package xtract

import scala.reflect.ClassTag

trait Diver {
  def dive[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, Reader[T])]]
  def dive[T](data: T, key: String, params: WriteParams[T]): (T, Writer[T])
}

object NestedDiver extends Diver {
  def dive[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, Reader[T])]] = {
    params.reader.get(data, key) match {
      case Some(data) if params.reader.accepts(data.getClass) => Some(Right((data.asInstanceOf[T], params.reader)))
      case Some(value) => Some(Left(value))
      case None => None
    }
  }

  def dive[T](data: T, key: String, params: WriteParams[T]): (T, Writer[T]) = {
    val writer = params.writer
    val reader = params.reader

    reader.get(data, key) match {
      case Some(data) if reader.accepts(data.getClass) => {
        (data.asInstanceOf[T], writer)
      }
      case Some(value) => {
        throw new Exception(s"write dive error: got $value")
      }
      case None => {
        val nested = writer.create
        writer.put(data, key, nested)
        (nested, writer)
      }
    }
  }
}

case class FlatDiver(separator: String) extends Diver {
  def dive[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, (T, Reader[T])]] = {
    Some(Right((data.asInstanceOf[T], PrefixedReader(key + separator, params.reader))))
  }

  def dive[T](data: T, key: String, params: WriteParams[T]): (T, Writer[T]) = {
    (data, PrefixedWriter(key + separator, params.writer))
  }
}