package krvoje.znjtorrent

import org.specs2._

class BEncodingTest extends mutable.Specification {

  "Strings should be deserialised correctly" >> {
    BEDecoder(BEString("kuki tuki").encoded).decode ==== BEString("kuki tuki")
    BEDecoder(BEString("čmrljenje").encoded).decode ==== BEString("čmrljenje")
    BEDecoder(BEString("").encoded).decode ==== BEString("")
  }

  "Ints should be deserialized correctly" >> {
    BEDecoder(BEInt(-123).encoded).decode ==== BEInt(-123)
    BEDecoder(BEInt(-12).encoded).decode ==== BEInt(-12)
    BEDecoder(BEInt(-1).encoded).decode ==== BEInt(-1)
    BEDecoder(BEInt(0).encoded).decode ==== BEInt(0)
    BEDecoder(BEInt(1).encoded).decode ==== BEInt(1)
    BEDecoder(BEInt(12).encoded).decode ==== BEInt(12)
    BEDecoder(BEInt(123).encoded).decode ==== BEInt(123)

    BEDecoder("i-123e").decode ==== BEInt(-123)
    BEDecoder("i-12e").decode ==== BEInt(-12)
    BEDecoder("i-1e").decode ==== BEInt(-1)
    BEDecoder("i-0e").decode ==== BEInt(0)
    BEDecoder("i0e").decode ==== BEInt(0)
    BEDecoder("i-0e").decode ==== BEInt(0)
    BEDecoder("i+0e").decode ==== BEInt(0)

    BEDecoder("i1e").decode ==== BEInt(1)
    BEDecoder("i12e").decode ==== BEInt(12)
    BEDecoder("i123e").decode ==== BEInt(123)

    BEDecoder("i+1e").decode ==== BEInt(1)
    BEDecoder("i+12e").decode ==== BEInt(12)
    BEDecoder("i+123e").decode ==== BEInt(123)
  }

  "Lists should be deserialized correctly" >> {
    BEDecoder(BEList().encoded).decode ==== BEList()
    BEDecoder(BEList(BEString("123"), BEString("")).encoded).decode ==== BEList(BEString("123"), BEString(""))
    BEDecoder(BEList(BEString("123"), BEInt(123)).encoded).decode ==== BEList(BEString("123"), BEInt(123))
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

    BEDecoder(dict.encoded).decode ==== dict
    BEDecoder(dictBig.encoded).decode ==== dictBig
    BEDecoder(dictBigger.encoded).decode ==== dictBigger
  }

  "Strings should be serialized correctly" >> {
    BEString("kuki tuki").encoded ==== "9:kuki tuki"
    BEString("").encoded ==== "0:"
    BEString("čmrljenje").encoded ==== "9:čmrljenje"
  }

  "Ints should be serialized correctly" >> {
    BEInt(123).encoded ==== "i123e"
    BEInt(12).encoded ==== "i12e"
    BEInt(0).encoded ==== "i0e"
  }

  "Lists should be serialized correctly" >> {
    BEList(BEString("123"), BEInt(123)).encoded ==== "l3:123i123ee"
  }

}