package xtract


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