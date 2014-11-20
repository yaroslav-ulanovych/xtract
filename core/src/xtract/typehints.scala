package xtract



trait TypeHintLocationStrategy {
  def getTypeHint[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, String]]
  def putTypeHint[T](data: T, key: String, typeHint: String, params: WriteParams[T])
}

case class BelowTypeHintLocationStrategy(typeHintFieldName: FieldName) extends TypeHintLocationStrategy {
  def getTypeHint[T](data: T, key: String, params: ReadParams[T]): Option[Either[Any, String]] = {
    params.diver.dive(data, key, params) match {
      case Some(Right((data, reader))) => {
        reader.get(data, params.fnc.apply(typeHintFieldName)) match {
          case Some(typeHint: String) => Some(Right(typeHint))
          case Some(value) => Some(Left(value))
          case None => None
        }
      }
      case Some(Left(_)) => None
      case None => None
    }
  }

  def putTypeHint[T](data: T, key: String, typeHint: String, params: WriteParams[T]) {
    val (nestedData, writer) = params.diver.dive(data, key, params)
    writer.put(nestedData, params.fnc.apply(typeHintFieldName), typeHint)
  }
}

trait TypeHintNamingStrategy {
  def getTypeHint(entity: AbstractObj): FieldName
  def guessType(klass: Class[_], typeHint: String): Option[Class[_]]

  def getTypeHint(entity: AbstractObj, fnc: FieldNamingConvention): String = {
    fnc.apply(getTypeHint(entity).words)
  }
}

object SamePackageTypeHintNamingStrategy extends TypeHintNamingStrategy {

  def getTypeHint(entity: AbstractObj): FieldName = {
    val x = entity.abstractClassName
    val y = entity.getClass.getSimpleName
    val typeHint = if (y.endsWith(x)) {
      y.substring(0, y.length - x.length)
    } else {
      y
    }
    FieldName(Utils.splitFieldNameIntoParts(typeHint))
  }

  def guessType(klass: Class[_], typeHint: String): Option[Class[_]] = {
    val pkg = klass.getPackage.getName
    (
      ClassUtils.forName(s"$pkg.${typeHint}"),
      ClassUtils.forName(s"$pkg.${typeHint}${klass.getSimpleName}")
    ) match {
      case (Some(x), _) => Some(x)
      case (None, Some(x)) => Some(x)
      case (None, None) => None
    }
  }

  def guessType(field: Entity#EmbeddedPolymorphicField[_], typeHint: String): Option[Class[_]] = {
    val abstractClass = field.classTag.runtimeClass.asInstanceOf[Class[AbstractObj]]
    val pkg = abstractClass.getPackage
    (
      ClassUtils.forName[AbstractObj](pkg, typeHint, ""),
      ClassUtils.forName[AbstractObj](pkg, typeHint, abstractClass.getSimpleName)
      ) match {
      case (Some(x), _) => Some(x)
      case (None, Some(x)) => Some(x)
      case (None, None) => None
    }
  }
}