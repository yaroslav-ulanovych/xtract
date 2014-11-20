package xtract


object DefaultReadParams extends ReadParams(
  reader = MapReader,
  diver = NestedDiver,
  layoutOld = NestedLayoutOld,
  fnc = LowerCamelCase.noDelimiter,
  thls = BelowTypeHintLocationStrategy("type"),
  thns = SamePackageTypeHintNamingStrategy,
  converters = Seq()
)

case class ReadParams[-T](
  reader: Reader[T],
  diver: Diver,
  layoutOld: LayoutOld,
  fnc: FieldNamingConvention,
  thns: TypeHintNamingStrategy,
  thls: TypeHintLocationStrategy,
  converters: Seq[Converter[_, _]]
)
{
  def +[U](x: Reader[U]) = copy(reader = x)
  def +(x: Diver) = copy(diver = x)
  def +(x: LayoutOld) = copy(layoutOld = x)
  def +(x: Converter[_, _]) = copy(converters = converters :+ x)
  def +(x: FieldNamingConvention) = copy(fnc = x)
}

case class WriteParams[T](
  writer: Writer[T],
  reader: Reader[T],
  diver: Diver,
  fnc: FieldNamingConvention,
  layoutOld: LayoutOld,
  thns: TypeHintNamingStrategy,
  thls: TypeHintLocationStrategy,
  fieldsLayout: FieldsLayout,
  allowedClasses: Seq[Class[_]],
  converters: Seq[Converter[_, _]]
) {
  def +(x: LayoutOld) = copy(layoutOld = x)
  def +(x: Converter[_, _]) = copy(converters = converters :+ x)
  def +(x: Writer[T]) = copy(writer = x)
  def +(x: Diver) = copy(diver = x)
  def +(x: FieldsLayout) = copy(fieldsLayout = x)

  def classAllowed(klass: Class[_]) = allowedClasses.contains(klass)

  def findConverterFrom(klass: Class[_]) = converters.find(_.canConvertTo(klass))
}

object DefaultWriteParams extends WriteParams(
  writer = MapWriter,
  reader = MapReader,
  diver = NestedDiver,
  fnc = LowerCamelCase.noDelimiter,
  layoutOld = NestedLayoutOld,
  thns = SamePackageTypeHintNamingStrategy,
  thls = BelowTypeHintLocationStrategy("type"),
  fieldsLayout = SingleLevelFieldsLayout,
  allowedClasses = Seq(
    classOf[Int], classOf[Long], classOf[Float], classOf[Double], classOf[Boolean], classOf[String],
    classOf[java.lang.Integer], classOf[java.lang.Long], classOf[java.lang.Float], classOf[java.lang.Double], classOf[java.lang.Boolean]
  ),
  converters = Seq()
)