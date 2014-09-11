package xtract

class NotCompanionObjectException(prefix: String, val reason: String) extends Exception(
  s"$prefix seems not to be a companion object, cause $reason"
) {

  def this(obj: AnyRef, reason: String) {
    this(s"${obj.getClass.getName}($obj)", reason)
  }

  def this(klass: Class[_], reason: String) {
    this(klass.getName, reason)
  }
}

case class CompanionObjectInstantiator(companionObject: AnyRef) {
  val companionObjectClass = companionObject.getClass
  
  val companionObjectClassName = companionObjectClass.getName
  
  def notCompanionObjectCause(reason: String) = throw new NotCompanionObjectException(companionObject, reason)
  
  val classNameWithoutDollar = companionObjectClassName.endsWith("$") match {
    case true => companionObjectClassName.dropRight(1)
    case false => notCompanionObjectCause(s"it's class name doesn't end with a dollar sign")
  }
  
  val endsWithNumberRegex = """^(.+)(\d+)$""".r
  
  val className = classNameWithoutDollar match {
    case endsWithNumberRegex(prefix, suffix) => prefix + (suffix.toInt - 1)
    case _ => classNameWithoutDollar
  }
  
  val klass = ClassUtils.forName(className).getOrElse {
    notCompanionObjectCause(s"it's companion class $className not found")
  }
  
  val applyMethods = companionObjectClass.getMethods.filter(_.getName == "apply")
  
  val applyMethod = applyMethods.find(_.getReturnType == klass) getOrElse {
    notCompanionObjectCause(s"it has no apply method that returns $className among ${applyMethods.toList}")
  }
 
  val argTypes = applyMethod.getParameterTypes.toSeq
}

object Instantiator {
  def apply(obj: AnyRef) = CompanionObjectInstantiator(obj)
}
