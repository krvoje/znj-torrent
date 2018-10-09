package krvoje.znjtorrent.bencoding

import akka.util.ByteString
import org.specs2._

class BEncodingTest extends mutable.Specification {

  "Strings should be deserialised correctly" >> {
    BEDeserializer(BEString("kuki tuki").serialized).decode ==== BEString("kuki tuki")
    BEDeserializer(BEString("čmrljenje").serialized).decode ==== BEString("čmrljenje")
    BEDeserializer(BEString("").serialized).decode ==== BEString("")
  }

  "Ints should be deserialized correctly" >> {
    BEDeserializer(BEInt(-123).serialized).decode ==== BEInt(-123)
    BEDeserializer(BEInt(-12).serialized).decode ==== BEInt(-12)
    BEDeserializer(BEInt(-1).serialized).decode ==== BEInt(-1)
    BEDeserializer(BEInt(0).serialized).decode ==== BEInt(0)
    BEDeserializer(BEInt(1).serialized).decode ==== BEInt(1)
    BEDeserializer(BEInt(12).serialized).decode ==== BEInt(12)
    BEDeserializer(BEInt(123).serialized).decode ==== BEInt(123)

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
    BEDeserializer(BEList().serialized).decode ==== BEList()
    BEDeserializer(BEList(BEString("123"), BEString("")).serialized).decode ==== BEList(BEString("123"), BEString(""))
    BEDeserializer(BEList(BEString("123"), BEInt(123)).serialized).decode ==== BEList(BEString("123"), BEInt(123))
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

    BEDeserializer(dict.serialized).decode ==== dict
    BEDeserializer(dictBig.serialized).decode ==== dictBig
    BEDeserializer(dictBigger.serialized).decode ==== dictBigger
  }

  "Strings should be serialized correctly" >> {
    BEString("kuki tuki").serialized ==== "9:kuki tuki"
    BEString("").serialized ==== "0:"
    BEString("čmrljenje").serialized ==== "9:čmrljenje"
  }

  "Ints should be serialized correctly" >> {
    BEInt(123).serialized ==== "i123e"
    BEInt(12).serialized ==== "i12e"
    BEInt(0).serialized ==== "i0e"
  }

  "Lists should be serialized correctly" >> {
    BEList(BEString("123"), BEInt(123)).serialized ==== "l3:123i123ee"
  }

  implicit def strToByteString(string: String): ByteString = ByteString(string)

}