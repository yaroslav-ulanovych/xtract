package xtract

case class FieldName(words: List[String]) {
  def render(fnc: FieldNamingConvention): String = fnc.apply(words)

  override def toString: String = {
    s"FieldName(${words.mkString(", ")})"
  }
}