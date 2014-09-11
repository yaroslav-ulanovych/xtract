package xtract


object DefaultReadParams extends ReadParams(
  reader = MapReader,
  layout = NestedLayout,
  fnc = LowerCamelCase.noDelimiter,
  thl = BelowTypeHintLocation(List("type")),
  thns = SamePackageTypeHintNamingStrategy,
  converters = Seq(JavaEnumConverter)
)

case class ReadParams[-T](
  reader: Reader[T],
  layout: Layout,
  fnc: FieldNamingConvention,
  thl: TypeHintLocation,
  thns: TypeHintNamingStrategy,
  converters: Seq[Converter]
)
{
  def +[U](x: Reader[U]) = copy(reader = x)
  def +(x: Layout) = copy(layout = x)
  def +(x: TypeHintLocation) = copy(thl = x)
  def +(x: Converter) = copy(converters = converters :+ x)
  def +(x: FieldNamingConvention) = copy(fnc = x)
}

case class WriteParams[T](
  writer: Writer[T],
  reader: Reader[T],
  fnc: FieldNamingConvention,
  layout: Layout,
  thl: TypeHintLocation,
  thns: TypeHintNamingStrategy
) {
  def +(x: Layout) = copy(layout = x)
  def +(x: TypeHintLocation) = copy(thl = x)
}

object DefaultWriteParams extends WriteParams(
  writer = MapWriter,
  reader = MapReader,
  fnc = LowerCamelCase.noDelimiter,
  layout = NestedLayout,
  thl = BelowTypeHintLocation(List("type")),
  thns = SamePackageTypeHintNamingStrategy
)