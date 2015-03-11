package xtract.jdbc

import xtract.{Reader, Writer}

import scala.collection.mutable.ArrayBuffer

/**
 * Was created to keep the order of fields written,
 * so that we can produce more readable sql statements.
 */
object ArrayBufferWriter extends Writer[ArrayBuffer[(String, Any)]] {
  def create = new ArrayBuffer[(String, Any)]()

  def put(data: ArrayBuffer[(String, Any)], key: String, value: Any) {
    data += ((key, value))
  }
}

/** Was created for writer. */
object ArrayBufferReader extends Reader[ArrayBuffer[(String, Any)]] {
  def get(data: ArrayBuffer[(String, Any)], key: String): Option[Any] = {
    data.find(_._1 == key).map(_._2)
  }
}

