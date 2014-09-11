package xtract

import scala.collection.mutable.{HashMap => MutableHashMap}

trait Writer[T] {
  def create: T
  def put(data: T, key: String, value: Any)
}

object MapWriter extends Writer[MutableHashMap[String, Any]] {
  def create = MutableHashMap[String, Any]()

  def put(data: MutableHashMap[String, Any], key: String, value: Any) {
    data.put(key, value)
  }
}