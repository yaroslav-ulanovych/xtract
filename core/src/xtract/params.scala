package xtract


object DefaultReadParams extends ReadParams(
  reader = MapReader,
  layout = NestedLayout,
  fnc = LowerCamelCase.noDelimiter,
  thns = SamePackageTypeHintNamingStrategy,
  converters = Seq(BuiltInConverters.IntegerToInt, JavaEnumConverter)
)

case class ReadParams[-T](
  reader: Reader[T],
  layout: Layout,
  fnc: FieldNamingConvention,
  thns: TypeHintNamingStrategy,
  converters: Seq[Converter]
)
{
  def +[U](x: Reader[U]) = copy(reader = x)
  def +(x: Layout) = copy(layout = x)
  def +(x: Converter) = copy(converters = converters :+ x)
  def +(x: FieldNamingConvention) = copy(fnc = x)
}

case class WriteParams[T](
  writer: Writer[T],
  reader: Reader[T],
  fnc: FieldNamingConvention,
  layout: Layout,
  thns: TypeHintNamingStrategy
) {
  def +(x: Layout) = copy(layout = x)
}

object DefaultWriteParams extends WriteParams(
  writer = MapWriter,
  reader = MapReader,
  fnc = LowerCamelCase.noDelimiter,
  layout = NestedLayout,
  thns = SamePackageTypeHintNamingStrategy
)