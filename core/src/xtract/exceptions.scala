package xtract

class InstantiationException(msg: String) extends Exception(msg)

object instantiationException {
  def apply(msg: String) = throw new InstantiationException(msg)
}

case class MissingFieldException(
  klass: Class[_],
  field: String,
  fieldType: Class[_],
  data: Any
) extends ReadException(
  klass,
  s"missing field $field: ${fieldType.getName} in $data"
)

case class MissingKeyException(data: Any, key: String) extends Exception(s"missing $key in $data")
case class BadKeyValueException(data: Any, key: String, value: Any, expected: String) extends Exception(s"bad key value for $key key, expected $expected, got ${value.getClass}($value)")

case class BadFieldValueException(
  klass: Class[_],
  field: String,
  fieldType: Class[_],
  value: Any,
  valueType: Class[_],
  converter: Option[Converter[_, _]]
) extends ReadException(
  klass,
  s"bad value for $field field, expected ${fieldType.getSimpleName}, got ${valueType.getSimpleName}($value), converter: $converter"
)

abstract class ReadException(klass: Class[_], msg: String) extends Exception(
  s"error reading ${klass.getName}: $msg"
)

case class MissingTypeHintException(
  klass: Class[_],
  field: String,
  fieldType: Class[_],
  data: Any
) extends ReadException(
  klass,
  s"missing type hint for $field field of ${fieldType.getName} type in $data"
)

case class BadTypeHintException(
  klass: Class[_],
  field: String,
  fieldType: Class[_],
  typeHint: String
) extends ReadException(
  klass,
  s"bad type hint for $field field of ${fieldType.getName}: $typeHint"
)

abstract class WriteException(data: Any, msg: String) extends Exception(
  s"error writing $data: $msg")

case class OtherWriteException(data: Any, msg: String) extends WriteException(data, msg)