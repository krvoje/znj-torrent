package krvoje.znjtorrent.bencoding

import scala.collection.mutable.ListBuffer

case class BEDeserializer(content: String) {
  var currentIndex: Int = 0

  private def current: Char = content(currentIndex)
  private def numeric(char: Char): Boolean = char >= '0' && char <= '9'

  private def next(): Char = {
    currentIndex += 1
    content(currentIndex)
  }

  def decode: BEValue = {
    current match {
      case BEValue.IntegerStart => decodeInt
      case BEValue.ListStart => decodeList
      case BEValue.DictionaryStart => decodeDictionary
      case _ => decodeString
    }
  }

  private def decodeInt: BEInt = {
    assert(current == BEValue.IntegerStart)
    val value = new StringBuilder()
    next
    while(current != BEValue.ValueEnd) {
      value.append(current)
      next
    }
    require(value.toString().matches("[+-]?[0-9]*"), s"ASCII values expected, instead got: '${value.toString()}'")
    if(value.head == '0' && value.size != 1) throw new InvalidBEInt(value.toString())
    assert(current == BEValue.ValueEnd)
    BEInt(value.toString().toInt)
  }

  private def stringLength: Int = {
    val value = new StringBuilder()

    while(current != BEValue.StringDelimiter) {
      value.append(current)
      if(!numeric(current)) throw new InvalidStringPrefix(current.toString)
      next
    }

    assert(current == BEValue.StringDelimiter)
    value.toInt
  }

  private def decodeString: BEString = {
    val length = stringLength
    BEString((for(i <- 0 until length) yield next).mkString)
  }

  private def decodeList: BEList = {
    assert(current == BEValue.ListStart)
    val res = ListBuffer[BEValue]()
    while(next != BEValue.ValueEnd) {
      res += decode
    }
    assert(current == BEValue.ValueEnd)
    BEList(res:_*)
  }

  private def decodeDictionary: BEDictionary = {
    assert(current == BEValue.DictionaryStart)
    val res = ListBuffer[(BEString, BEValue)]()
    next
    while (current != BEValue.ValueEnd) {
      val key = decodeString
      next
      val value = decode
      res += key -> value
      next
    }
    assert(current == BEValue.ValueEnd)
    BEDictionary(res:_*)
  }
}
