package xtract.var2

import xtract.{AbstractObj, Obj}

class Person extends Obj {
  val name = string
  val address = embedded[Address]
  val status = embedded[Status]
}

class Address extends Obj {
  val country = string
}

class Status extends AbstractObj

class Married extends Status {
  val partner = string
}