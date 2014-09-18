package xtract

import xtract.query.Query

trait CrudStorage {
  def create[T <: Entity](obj: T)
//  def create[T <: Obj with Id](obj: T): T
  def read[T <: Obj with Id: Manifest](id: T#Id): Option[T]
  def update[T <: Obj with Id](Obj: T): {def set(x: (T) => Unit): Unit}

  def select[T <: Obj](query: Query[T]): Option[T]

  /** Delete without where clause, deletes everything. */
  //def delete[T <: Obj: Manifest]()

  def delete[T <: Obj: Manifest](where: Query[T] = null)


  def inTransaction[T](doWork: => T): T
}