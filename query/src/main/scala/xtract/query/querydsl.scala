package xtract.query

import xtract.{AbstractObj, Entity, Obj}

abstract sealed class QueryClause


case class IsClause[V <: AbstractObj, U <: V](field: Entity#EmbeddedPolymorphicField[V], value: Class[U]) extends QueryClause {
  val x = value.newInstance()
  x.wasEmbeddedInto(field)
  def andInThere(f: U => QueryClause) = List(this, f(x))
}


case class SimpleFieldEqClause[E <: Entity, V](field: E#SimpleField[V], value: V) extends QueryClause

case class LinkFieldEqClause[E <: Entity, V](field: E#LinkField[V], value: V) extends QueryClause

case class CustomFieldEqClause[E <: Entity, V, U](field: E#CustomField[V, U], value: V) extends QueryClause




case class Query[T <: Entity: Manifest](meta: T, clauses: List[QueryClause]) {
  val e = manifest.runtimeClass.asInstanceOf[Class[T]].newInstance()
  def and(f: T => QueryClause) = Query(meta, clauses ++ List(f(e)))
}



case class SimpleFieldQueryClauseLeftOperand[T](field: Entity#SimpleField[T]) {
  def eqs(value: T) = SimpleFieldEqClause(field, value)
}

case class LinkFieldQueryClauseLeftOperand[T](field: Entity#LinkField[T]) {
  def eqs(value: T) = LinkFieldEqClause(field, value)
}

case class CustomFieldQueryClauseLeftOperand[T, U](field: Entity#CustomField[T, U]) {
  def eqs(value: T) = CustomFieldEqClause(field, value)
}



case class From[T <: Obj: Manifest]() {
  def where(f: T => QueryClause): Query[T] = {
    val meta = manifest.runtimeClass.newInstance().asInstanceOf[T]
    Query(meta, List(f(meta)))
  }
}


object QueryDsl {
  def where[T <: Entity: Manifest](f: T => List[QueryClause]): Query[T] = {
    val meta = manifest.runtimeClass.newInstance().asInstanceOf[T]
    Query(meta, f(meta))
  }

  def from[T <: Obj: Manifest] = From[T]

  implicit def singleWhereClauseToListOfThem(x: QueryClause) = List(x)

  implicit def simpleFieldToQueryClauseLeftOperand[T](x: Entity#SimpleField[T]) = SimpleFieldQueryClauseLeftOperand(x)
  implicit def linkFieldToQueryClauseLeftOperand[T](x: Entity#LinkField[T]) = LinkFieldQueryClauseLeftOperand(x)
  implicit def customFieldToQueryClauseLeftOperand[T, U](x: Entity#CustomField[T, U]) = CustomFieldQueryClauseLeftOperand(x)
}
