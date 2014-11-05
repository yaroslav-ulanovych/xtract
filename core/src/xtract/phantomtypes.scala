package xtract

sealed trait Uniqueness
abstract final class Unique extends Uniqueness
abstract final class NotUnique extends Uniqueness

sealed trait AutoIncness
abstract final class AutoInc extends AutoIncness
abstract final class NotAutoInc extends AutoIncness