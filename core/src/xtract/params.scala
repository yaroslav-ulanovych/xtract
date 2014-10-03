package xtract


object DefaultReadParams extends ReadParams(
  reader = MapReader,
  layout = NestedLayout,
  fnc = LowerCamelCase.noDelimiter,
  thns = SamePackageTypeHintNamingStrategy,
  converters = Seq()
)

case class ReadParams[-T](
  reader: Reader[T],
  layout: Layout,
  fnc: FieldNamingConvention,
  thns: TypeHintNamingStrategy,
  converters: Seq[Converter[_, _]]
)
{
  def +[U](x: Reader[U]) = copy(reader = x)
  def +(x: Layout) = copy(layout = x)
  def +(x: Converter[_, _]) = copy(converters = converters :+ x)
  def +(x: FieldNamingConvention) = copy(fnc = x)
}

case class WriteParams[T](
  writer: Writer[T],
  reader: Reader[T],
  fnc: FieldNamingConvention,
  layout: Layout,
  thns: TypeHintNamingStrategy,
  allowedClasses: Seq[Class[_]],
  converters: Seq[Converter[_, _]]
) {
  def +(x: Layout) = copy(layout = x)
  def +(x: Converter[_, _]) = copy(converters = converters :+ x)

  def classAllowed(klass: Class[_]) = allowedClasses.contains(klass)

  def findConverterFrom(klass: Class[_]) = converters.find(_.canConvertTo(klass))
}

object DefaultWriteParams extends WriteParams(
  writer = MapWriter,
  reader = MapReader,
  fnc = LowerCamelCase.noDelimiter,
  layout = NestedLayout,
  thns = SamePackageTypeHintNamingStrategy,
  allowedClasses = Seq(
    classOf[Int], classOf[Long], classOf[Float], classOf[Double], classOf[Boolean], classOf[String],
    classOf[java.lang.Integer], classOf[java.lang.Long], classOf[java.lang.Float], classOf[java.lang.Double], classOf[java.lang.Boolean]
  ),
  converters = Seq()
)