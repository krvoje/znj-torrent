package krvoje.znjtorrent.bencoding

import scala.collection.mutable.ListBuffer


trait BE {
  val encoded: String
}

trait BEValue[T] extends BE {
  val value: T
  val encoded: String
}


object BE {
  val StringDelimiter = ':'
  val IntegerStart = 'i'
  val ListStart = 'l'
  val DictionaryStart = 'd'
  val ValueEnd = 'e'
}

case class BEString(value: String) extends BEValue[String] {
  val encoded = s"${value.length}${BE.StringDelimiter}${value}"
}


case class BEInt(value: Int) extends BEValue[Int] {
  val encoded = s"i${value}e"
}

case class BEList(values: BE*) extends BE {
  val encoded = s"l${values.map(_.encoded).mkString("")}e"
}

case class BEDictionary(values: (BEString, BE)*) extends BE {
  val encoded =
    s"d${values.map(bevalue => s"${bevalue._1.encoded}${bevalue._2.encoded}").mkString("")}e"

  val dict: Map[String, BE] = values.map(v => v._1.value -> v._2).toMap

  def string(key: String): String = dict(key).asInstanceOf[BEString].value
  def stringOpt(key: String): Option[String] = dict.get(key).map(_.asInstanceOf[BEString].value)
  def int(key: String): Int = dict(key).asInstanceOf[BEInt].value
  def intOpt(key: String): Option[Int] = dict.get(key).map(_.asInstanceOf[BEInt].value)
}


case class BEDecoder(content: String) {
  var currentIndex: Int = 0

  private def current: Char = content.charAt(currentIndex)
  private def numeric(char: Char): Boolean = char >= '0' && char <= '9'

  private def next(): Char = {
    currentIndex += 1
    content.charAt(currentIndex)
  }

  def decode: BE = {
    current match {
      case BE.IntegerStart => int
      case BE.ListStart => list
      case BE.DictionaryStart => dictionary
      case _ => string
    }
  }

  def int: BEInt = {
    assert(current == BE.IntegerStart)
    val value = new StringBuilder()
    next
    while(current != BE.ValueEnd) {
      value.append(current)
      next
    }
    require(value.toString().matches("[+-]?[0-9]*"), s"ASCII values expected, instead got: '${value.toString()}'")
    if(value.head == '0' && value.size != 1) throw new InvalidBEInt(value.toString())
    assert(current == BE.ValueEnd)
    BEInt(value.toString().toInt)
  }

  private def stringLength: Int = {
    val value = new StringBuilder()

    while(current != BE.StringDelimiter) {
      value.append(current)
      if(!numeric(current)) throw new InvalidStringPrefix(current.toString)
      next
    }

    assert(current == BE.StringDelimiter)
    value.toInt
  }

  def string: BEString = {
    val length = stringLength
    val value = (for(i <- 0 until length) yield next).mkString("")
    BEString(value)
  }

  def list: BEList = {
    assert(current == BE.ListStart)
    val res = ListBuffer[BE]()
    while(next != BE.ValueEnd) {
      res += decode
    }
    assert(current == BE.ValueEnd)
    BEList(res:_*)
  }

  def dictionary: BEDictionary = {
    assert(current == BE.DictionaryStart)
    val res = ListBuffer[(BEString, BE)]()
    next
    while (current != BE.ValueEnd) {
      val key = string
      next
      val value = decode
      res += key -> value
      next
    }
    assert(current == BE.ValueEnd)
    BEDictionary(res:_*)
  }
}

// TODO: Fix these
class InvalidParam(message: String) extends Exception
class InvalidBEInt(message: String) extends InvalidParam(message)
class InvalidStringPrefix(message: String) extends InvalidParam(message)
class InvalidList(message: String, bEList: BEList) extends InvalidParam(message)