package rw.obj

import rw.FunSpec


class VarTest extends FunSpec {
  val person = new Person
  val address = new Address
  val status = new Married

  person.address := address

  person.status := status

  it("qname") {
    person.name.qname shouldBe List("name")
    address.country.qname shouldBe List("country")
    //status.partner.qname shouldBe List("married", "partner")
  }

  it("mpath") {
    person.name.mpath shouldBe List()
    address.country.mpath shouldBe List(person.address)
    status.partner.mpath shouldBe List(person.status)
  }

  it("abstractClass") {
    status.abstractClass shouldBe classOf[Status]
  }
}
