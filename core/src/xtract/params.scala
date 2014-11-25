package xtract


object DefaultReadParams extends ReadParams(
  reader = MapReader,
  diver = NestedDiver,
  fnc = LowerCamelCase.noDelimiter,
  thls = BelowTypeHintLocationStrategy("type"),
  thns = SamePackageTypeHintNamingStrategy,
  fieldsLayout = SimpleFieldsLocation,
  converters = Seq()
)

case class ReadParams[-T](
  reader: Reader[T],
  diver: Diver,
  fnc: FieldNamingConvention,
  thns: TypeHintNamingStrategy,
  thls: TypeHintLocationStrategy,
  fieldsLayout: FieldsLocation,
  converters: Seq[Converter[_, _]]
)
{
  def +[U](x: Reader[U]) = copy(reader = x)
  def +(x: Diver) = copy(diver = x)
  def +(x: Converter[_, _]) = copy(converters = converters :+ x)
  def +(x: FieldNamingConvention) = copy(fnc = x)
  def +(x: TypeHintLocationStrategy) = copy(thls = x)
  def +(x: FieldsLocation) = copy(fieldsLayout = x)
}

case class WriteParams[T](
  writer: Writer[T],
  reader: Reader[T],
  diver: Diver,
  fnc: FieldNamingConvention,
  thns: TypeHintNamingStrategy,
  thls: TypeHintLocationStrategy,
  fieldsLayout: FieldsLocation,
  allowedClasses: Seq[Class[_]],
  converters: Seq[Converter[_, _]]
) {
  def +(x: Converter[_, _]) = copy(converters = converters :+ x)
  def +(x: Writer[T]) = copy(writer = x)
  def +(x: Diver) = copy(diver = x)
  def +(x: FieldsLocation) = copy(fieldsLayout = x)
  def +(x: TypeHintLocationStrategy) = copy(thls = x)

  def classAllowed(klass: Class[_]) = allowedClasses.contains(klass)

  def findConverterFrom(klass: Class[_]) = converters.find(_.canConvertTo(klass))
}

object DefaultWriteParams extends WriteParams(
  writer = MapWriter,
  reader = MapReader,
  diver = NestedDiver,
  fnc = LowerCamelCase.noDelimiter,
  thns = SamePackageTypeHintNamingStrategy,
  thls = NearTypeHintLocationStrategy("type"),
  fieldsLayout = SimpleFieldsLocation,
  allowedClasses = Seq(
    classOf[Int], classOf[Long], classOf[Float], classOf[Double], classOf[Boolean], classOf[String],
    classOf[java.lang.Integer], classOf[java.lang.Long], classOf[java.lang.Float], classOf[java.lang.Double], classOf[java.lang.Boolean]
  ),
  converters = Seq()
)