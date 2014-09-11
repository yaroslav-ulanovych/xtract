package xtract.docs.embedded.concrete

import xtract.{DefaultReadParams, read, FunSuite}

// doc begin
// # <a name="embedded-concrete-classes-section">Embedded concrete classes</a>
// Case classes may contain other case classes in their fields,
// (I'll call them _complex_ as opposed to _flat_ case classes)
// for instance person may have an address
case class Person(name: String, address: Address)
case class Address(country: String, city: String)
// Obviously we could put address' fields among person's fields
case class PersonWithAddressFields(name: String, country: String, city: String)
// but that deprives us of the ability to reuse address class in other places
// and doesn't look like object-oriented.
// doc end

// def begin PersonWithTwoAddresses
case class PersonWithTwoAddresses(homeAddress: Address, businessAddress: Address)
// def end

class EmbeddedConcreteClassesSection extends FunSuite {
// doc begin
// Probably the first way to serialize a complex case class to some data structure that comes up to mind is
  val defaultData = Map(
    "name" -> "John",
    "address" -> Map(
      "country" -> "USA",
      "city" -> "NY"
    )
  )
// I'll call such layout a _default_ (couldn't come up with a better name, please let me know, if you could).
// But there are data structures that are _flat_, e.g. they can't hold another data structures among it's members,
// like jdbc result sets or command line arguments (which are string arrays).
  val flatData = Map(
  "name" -> "John",
  "address_country" -> "USA",
  "address_city" -> "NY"
  )
// I'll call such layout _flat_.
// Notice, that we have to include information about field,
// that contains embedded class, into names of fields of that class,
// to avoid possible name clashes,
// for instance if a person has two addresses - home and business,
// then we have two sets of address' fields and have to differentiate somehow which are which
// include PersonWithTwoAddresses
  val flatData2 = Map(
    "homeAddress_country" -> "USA",
    "homeAddress_city" -> "NY",
    "businessAddress_country" -> "China",
    "businessAddress_city" -> "Beijing"
  )
// There is a third possible layout, but it's reasonable for embedded polymorphic classes,
// so it will be discussed in a corresponding section.
// For embedded concrete classes you have two options
  import xtract.{FlatLayout, NestedLayout}
// I won't dive into implementation details, cause I believe all possible cases are covered by built-in layouts,
// and there is no need for users to define their owns. If it's not so, please let me know and we'll fix that.
//
// Default layout is, well, the default
// doc end
  test("default layout should be the default") {
// doc begin
    DefaultReadParams.layout shouldBe NestedLayout
// doc end
  }

  test("default layout") {
// so you don't need to setup anything
    val person = read[Person] from defaultData
    person shouldBe Person("John", Address("USA", "NY"))
// doc end
  }
  
  test("flat layout") {
// doc begin
// Flat layout takes a single parameter that is a separator between field names that form a final field name.
    implicit val params = DefaultReadParams + FlatLayout("_")
    val person = read[Person] from flatData
    person shouldBe Person("John", Address("USA", "NY"))
// doc end
  }

  test("flat layout 2") {
    implicit val params = DefaultReadParams + FlatLayout("_")
// doc begin
    val person = read[PersonWithTwoAddresses] from flatData2
    person shouldBe PersonWithTwoAddresses(Address("USA", "NY"), Address("China", "Beijing"))
// doc end
  }
}
