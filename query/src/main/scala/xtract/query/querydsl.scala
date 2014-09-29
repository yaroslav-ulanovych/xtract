package xtract.query

import xtract._

import scala.reflect.ClassTag



sealed trait QueryResult[U <: Uniqueness] {
  type Result[_]
}

class UniqueQueryResult extends QueryResult[Unique] {
  type Result[T] = Option[T]
}

class NotUniqueQueryResult extends QueryResult[NotUnique] {
  type Result[T] = List[T]
}



abstract sealed class QueryClause[Uniqueness]


//case class IsClause[V <: AbstractObj, U <: V](field: Entity#EmbeddedPolymorphicField[V], value: Class[U]) extends QueryClause {
//  val x = value.newInstance()
//  x.wasEmbeddedInto(field)
//  def andInThere(f: U => QueryClause) = List(this, f(x))
//}


case class SimpleFieldEqClause[E <: Entity, V, U <: Uniqueness](field: E#SimpleField[V, U], value: V) extends QueryClause[U]
case class SimpleFieldInClause[E <: Entity, V](field: E#SimpleField[V, _ <: Uniqueness], values: Traversable[V]) extends QueryClause[NotUnique]

//case class LinkFieldEqClause[E <: Entity, V](field: E#LinkField[V], value: V) extends QueryClause
//
//case class CustomFieldEqClause[E <: Entity, V, U](field: E#CustomField[V, U], value: V) extends QueryClause




case class Query[T <: Entity: ClassTag, U <: Uniqueness](meta: T, clauses: List[QueryClause[_]]) {
  val klass = implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]
  val e = klass.newInstance()
//  def and(f: T => QueryClause) = Query(meta, clauses ++ List(f(e)))
}

object Query {
  implicit def fromFrom[T <: Obj: ClassTag](from: From[T]) = {
    val klass = implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]
    Query[T, NotUnique](klass.newInstance(), List())
  }
}



case class SimpleFieldQueryClauseLeftOperand[T, U <: Uniqueness](field: Entity#SimpleField[T, U]) {
  def eqs(value: T) = SimpleFieldEqClause(field, value)
  def in(values: T*) = SimpleFieldInClause[Entity, T](field, values)
}

//case class LinkFieldQueryClauseLeftOperand[T](field: Entity#LinkField[T]) {
//  def eqs(value: T) = LinkFieldEqClause(field, value)
//}
//
//case class CustomFieldQueryClauseLeftOperand[T, U](field: Entity#CustomField[T, U]) {
//  def eqs(value: T) = CustomFieldEqClause(field, value)
//}
//


case class From[T <: Obj: ClassTag]() {
  def where[U <: Uniqueness](f: T => QueryClause[U]): Query[T, U] = {
    val meta = implicitly[ClassTag[T]].runtimeClass.newInstance().asInstanceOf[T]
    Query(meta, List(f(meta)))
  }
}


object QueryDsl {

  def from[T <: Obj: ClassTag] = From[T]

//  implicit def singleWhereClauseToListOfThem(x: QueryClause) = List(x)

  implicit def simpleFieldToQueryClauseLeftOperand[T, U <: Uniqueness](field: Entity#SimpleField[T, U]) = SimpleFieldQueryClauseLeftOperand(field)

  implicit def xx = new UniqueQueryResult
  implicit def xxx = new NotUniqueQueryResult

//  implicit def simpleFieldToQueryClauseLeftOperand[T, U](x: Entity#SimpleField[T, U]) = SimpleFieldQueryClauseLeftOperand(x)
//  implicit def linkFieldToQueryClauseLeftOperand[T](x: Entity#LinkField[T]) = LinkFieldQueryClauseLeftOperand(x)
//  implicit def customFieldToQueryClauseLeftOperand[T, U](x: Entity#CustomField[T, U]) = CustomFieldQueryClauseLeftOperand(x)
}
