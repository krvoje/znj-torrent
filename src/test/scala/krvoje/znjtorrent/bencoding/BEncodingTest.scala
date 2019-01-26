package krvoje.znjtorrent.bencoding

import akka.util.ByteString
import org.specs2._

class BEncodingTest extends mutable.Specification {

  "Strings should be deserialised correctly" >> {
    BEDeserializer(BEValueSerializer.serialize(BEString("kuki tuki"))).decode ==== BEString("kuki tuki")
    BEDeserializer(BEValueSerializer.serialize(BEString("čmrljenje"))).decode ==== BEString("čmrljenje")
    BEDeserializer(BEValueSerializer.serialize(BEString(""))).decode ==== BEString("")
  }

  "Ints should be deserialized correctly" >> {
    BEDeserializer(BEValueSerializer.serialize(BEInt(-123))).decode ==== BEInt(-123)
    BEDeserializer(BEValueSerializer.serialize(BEInt(-12))).decode ==== BEInt(-12)
    BEDeserializer(BEValueSerializer.serialize(BEInt(-1))).decode ==== BEInt(-1)
    BEDeserializer(BEValueSerializer.serialize(BEInt(0))).decode ==== BEInt(0)
    BEDeserializer(BEValueSerializer.serialize(BEInt(1))).decode ==== BEInt(1)
    BEDeserializer(BEValueSerializer.serialize(BEInt(12))).decode ==== BEInt(12)
    BEDeserializer(BEValueSerializer.serialize(BEInt(123))).decode ==== BEInt(123)

    BEDeserializer("i-123e").decode ==== BEInt(-123)
    BEDeserializer("i-12e").decode ==== BEInt(-12)
    BEDeserializer("i-1e").decode ==== BEInt(-1)
    BEDeserializer("i-0e").decode ==== BEInt(0)
    BEDeserializer("i0e").decode ==== BEInt(0)
    BEDeserializer("i-0e").decode ==== BEInt(0)
    BEDeserializer("i+0e").decode ==== BEInt(0)

    BEDeserializer("i1e").decode ==== BEInt(1)
    BEDeserializer("i12e").decode ==== BEInt(12)
    BEDeserializer("i123e").decode ==== BEInt(123)

    BEDeserializer("i+1e").decode ==== BEInt(1)
    BEDeserializer("i+12e").decode ==== BEInt(12)
    BEDeserializer("i+123e").decode ==== BEInt(123)
  }

  "Lists should be deserialized correctly" >> {
    BEDeserializer(BEValueSerializer.serialize(BEList())).decode ==== BEList()
    BEDeserializer(BEValueSerializer.serialize(BEList(BEString("123"), BEString("")))).decode ==== BEList(BEString("123"), BEString(""))
    BEDeserializer(BEValueSerializer.serialize(BEList(BEString("123"), BEInt(123)))).decode ==== BEList(BEString("123"), BEInt(123))
  }

  "Dictionaries should be deserialized correctly" >> {
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

    BEDeserializer(BEValueSerializer.serialize(dict)).decode ==== dict
    BEDeserializer(BEValueSerializer.serialize(dictBig)).decode ==== dictBig
    BEDeserializer(BEValueSerializer.serialize(dictBigger)).decode ==== dictBigger
  }

  "Strings should be serialized correctly" >> {
    BEValueSerializer.serialize(BEString("kuki tuki")) ==== "9:kuki tuki"
    BEValueSerializer.serialize(BEString("")) ==== "0:"
    BEValueSerializer.serialize(BEString("čmrljenje")) ==== "9:čmrljenje"
  }

  "Ints should be serialized correctly" >> {
    BEValueSerializer.serialize(BEInt(123)) ==== "i123e"
    BEValueSerializer.serialize(BEInt(12)) ==== "i12e"
    BEValueSerializer.serialize(BEInt(0)) ==== "i0e"
  }

  "Lists should be serialized correctly" >> {
    BEValueSerializer.serialize(BEList(BEString("123"), BEInt(123))) ==== "l3:123i123ee"
  }

  implicit def strToByteString(string: String): ByteString = ByteString(string)

}