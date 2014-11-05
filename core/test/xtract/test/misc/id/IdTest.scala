package xtract.test.misc.id

import xtract.{Id, Obj}

class User extends Obj with Id {
  type Id = String
  val id = string.unique
}

class IdTest {

}
