import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary

import frdomain.ch3.lens.{Address, AddressLenses}

trait Generators {
  implicit val addressArbitrary: Arbitrary[Address] = 
    Arbitrary {
      for {
        no <- Arbitrary.arbitrary[String]
        street <- Arbitrary.arbitrary[String]
        city <- Arbitrary.arbitrary[String]
        state <- Arbitrary.arbitrary[String]
        zip <- Arbitrary.arbitrary[String]
      } yield Address(no, street, city, state, zip)
    }
}

class NoLensSpec extends Properties("No Lens") with AddressLenses with Generators {
  property("Identity") = forAll((address: Address) => {
    val no = noLens.get(address)
    val address2 = noLens.set(address, no)
    address == address2
  })

  property("Retention") = forAll((address: Address, no: String) => {
    val address2 = noLens.set(address, no)
    val no2 = noLens.get(address2)
    no == no2
  })

  property("Double set") = forAll((address: Address, no1: String, no2: String) => {
    val addressAfterFirstSet = noLens.set(address, no1)
    val addressAfterSecondSet = noLens.set(addressAfterFirstSet, no2)
    val noAfterSecondSet = noLens.get(addressAfterSecondSet)
    noAfterSecondSet == no2
  })
}
