package xtract

abstract sealed trait Uniqueness
abstract sealed class Unique extends Uniqueness
abstract sealed class NotUnique extends Uniqueness