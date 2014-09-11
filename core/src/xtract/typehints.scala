package xtract


trait TypeHintNamingStrategy {
  def getTypeHint(entity: AbstractObj): String
  def guessType(klass: Class[_], typeHint: String): Option[Class[_]]
}

object SamePackageTypeHintNamingStrategy extends TypeHintNamingStrategy {

  def getTypeHint(entity: AbstractObj): String = {
    val x = entity.abstractClassName
    val y = entity.getClass.getSimpleName
    if (y.endsWith(x)) {
      y.substring(0, y.length - x.length)
    } else {
      y
    }
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