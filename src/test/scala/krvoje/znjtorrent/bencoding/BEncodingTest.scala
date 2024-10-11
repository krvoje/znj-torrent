package krvoje.znjtorrent.bencoding

import org.apache.pekko.util.ByteString
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BEncodingTest extends AnyFlatSpec with Matchers {

  "Strings" should "be deserialized correctly" in {
    BEDeserializer(BEValueSerializer.serialize(BEString("kuki tuki"))).decode shouldEqual BEString("kuki tuki")
    BEDeserializer(BEValueSerializer.serialize(BEString("čmrljenje"))).decode shouldEqual BEString("čmrljenje")
    BEDeserializer(BEValueSerializer.serialize(BEString(""))).decode shouldEqual BEString("")
  }

  "Ints" should "be deserialized correctly" in {
    BEDeserializer(BEValueSerializer.serialize(BEInt(-123))).decode shouldEqual BEInt(-123)
    BEDeserializer(BEValueSerializer.serialize(BEInt(-12))).decode shouldEqual BEInt(-12)
    BEDeserializer(BEValueSerializer.serialize(BEInt(-1))).decode shouldEqual BEInt(-1)
    BEDeserializer(BEValueSerializer.serialize(BEInt(0))).decode shouldEqual BEInt(0)
    BEDeserializer(BEValueSerializer.serialize(BEInt(1))).decode shouldEqual BEInt(1)
    BEDeserializer(BEValueSerializer.serialize(BEInt(12))).decode shouldEqual BEInt(12)
    BEDeserializer(BEValueSerializer.serialize(BEInt(123))).decode shouldEqual BEInt(123)

    BEDeserializer("i-123e").decode shouldEqual BEInt(-123)
    BEDeserializer("i-12e").decode shouldEqual BEInt(-12)
    BEDeserializer("i-1e").decode shouldEqual BEInt(-1)
    BEDeserializer("i0e").decode shouldEqual BEInt(0)
    BEDeserializer("i-0e").decode shouldEqual BEInt(0)
    BEDeserializer("i+0e").decode shouldEqual BEInt(0)

    BEDeserializer("i1e").decode shouldEqual BEInt(1)
    BEDeserializer("i12e").decode shouldEqual BEInt(12)
    BEDeserializer("i123e").decode shouldEqual BEInt(123)

    BEDeserializer("i+1e").decode shouldEqual BEInt(1)
    BEDeserializer("i+12e").decode shouldEqual BEInt(12)
    BEDeserializer("i+123e").decode shouldEqual BEInt(123)
  }

  "Lists" should "be deserialized correctly" in {
    BEDeserializer(BEValueSerializer.serialize(BEList())).decode shouldEqual BEList()
    BEDeserializer(BEValueSerializer.serialize(BEList(BEString("123"), BEString("")))).decode shouldEqual BEList(BEString("123"), BEString(""))
    BEDeserializer(BEValueSerializer.serialize(BEList(BEString("123"), BEInt(123)))).decode shouldEqual BEList(BEString("123"), BEInt(123))
  }

  "Dictionaries" should "be deserialized correctly" in {
    val dict = BEDictionary(
      BEString("") -> BEString(""),
      BEString("Info") -> BEString("Nema info"),
      BEString("Broj") -> BEInt(3),
      BEString("Lista") -> BEList()
    )

    val dictBig = BEDictionary(
      BEString("") -> BEString(""),
      BEString("Info") -> BEString("Nema info"),
      BEString("Broj") -> BEInt(3),
      BEString("Dict") -> dict
    )

    val list = BEList(dict, dict)
    val dictBigger = BEDictionary(
      BEString("Listetina") -> list
    )

    BEDeserializer(BEValueSerializer.serialize(dict)).decode shouldEqual dict
    BEDeserializer(BEValueSerializer.serialize(dictBig)).decode shouldEqual dictBig
    BEDeserializer(BEValueSerializer.serialize(dictBigger)).decode shouldEqual dictBigger
  }

  "Strings" should "be serialized correctly" in {
    BEValueSerializer.serialize(BEString("kuki tuki")) shouldEqual "9:kuki tuki"
    BEValueSerializer.serialize(BEString("")) shouldEqual "0:"
    BEValueSerializer.serialize(BEString("čmrljenje")) shouldEqual "9:čmrljenje"
  }

  "Ints" should "be serialized correctly" in {
    BEValueSerializer.serialize(BEInt(123)) shouldEqual "i123e"
    BEValueSerializer.serialize(BEInt(12)) shouldEqual "i12e"
    BEValueSerializer.serialize(BEInt(0)) shouldEqual "i0e"
  }

  "Lists" should "be serialized correctly" in {
    BEValueSerializer.serialize(BEList(BEString("123"), BEInt(123))) shouldEqual "l3:123i123ee"
  }

  implicit def strToByteString(string: String): ByteString = ByteString(string)
}