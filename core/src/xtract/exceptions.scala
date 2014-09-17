package xtract

class InstantiationException(msg: String) extends Exception(msg)

object instantiationException {
  def apply(msg: String) = throw new InstantiationException(msg)
}

case class MissingFieldException(
  klass: Class[_],
  field: String,
  data: Any
) extends Exception(
  s"error reading ${klass.getName}, missing field $field in $data"
)

case class BadFieldValueException(
  klass: Class[_],
  field: String,
  fieldType: Class[_],
  value: Any,
  valueType: Class[_],
  converter: Option[Converter]
) extends ReadException(
  klass,
  s"bad value for $field field of ${fieldType.getSimpleName} type: ${valueType.getSimpleName}($value), converter: $converter"
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