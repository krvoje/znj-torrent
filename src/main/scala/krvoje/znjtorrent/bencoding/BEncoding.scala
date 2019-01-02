package krvoje.znjtorrent.bencoding

import scala.collection.mutable.ListBuffer

trait BEValue {
  type T
  val value: T
  val serialized: String
}

trait BEValueSerializer[T] {
  def serialize(value: T): String
}
object BEValueSerializer {
  def apply[T](implicit enc: BEValueSerializer[T]): BEValueSerializer[T] = enc
  def createSerializer[T](f: T => String) = new BEValueSerializer[T] {
    override def serialize(value: T): String = f(value)
  }
}

object BE {
  val StringDelimiter = ':'
  val IntegerStart = 'i'
  val ListStart = 'l'
  val DictionaryStart = 'd'
  val ValueEnd = 'e'
}

case class BEString(value: String) extends BEValue {
  type T = String

  val serialized = s"${value.length}${BE.StringDelimiter}${value}"
}

object BEString {
  implicit val beStringSerializer = BEValueSerializer.createSerializer[String](value =>
    s"${value.length}${BE.StringDelimiter}${value}")
}


case class BEInt(value: Int) extends BEValue {
  type T = Int

  val serialized = s"${BE.IntegerStart}${value}${BE.ValueEnd}"
}
object BEInt {
  implicit val beIntSerializer = BEValueSerializer.createSerializer[Int](value =>
    s"${BE.IntegerStart}${value}${BE.ValueEnd}")
}

case class BEList(value: BEValue*) extends BEValue {
  type T = Seq[BEValue]

  val serialized = s"${BE.ListStart}${value.map(_.serialized).mkString("")}${BE.ValueEnd}"
}
object BEList {
  implicit def beListSerializer = {
    BEValueSerializer.createSerializer[Seq[BEValue]]{
      values => s"${BE.ListStart}${values.map(_.serialized).mkString("")}${BE.ValueEnd}"
    }
  }
}

// TODO: Use a dict instead of Seq of tuples
case class BEDictionary(value: (BEString, BEValue)*) extends BEValue {
  type T = Seq[(BEString, BEValue)]
  val serialized =
    s"${BE.DictionaryStart}${value.map(bevalue => s"${bevalue._1.serialized}${bevalue._2.serialized}").mkString("")}${BE.ValueEnd}"

  val dict: Map[String, BEValue] = value.map(v => v._1.value -> v._2).toMap

  def string(key: String): String = dict(key).asInstanceOf[BEString].value
  def stringOpt(key: String): Option[String] = dict.get(key).map(_.asInstanceOf[BEString].value)
  def int(key: String): Int = dict(key).asInstanceOf[BEInt].value
  def intOpt(key: String): Option[Int] = dict.get(key).map(_.asInstanceOf[BEInt].value)
}
object BEDictionary {
  implicit def beDictionarySerializer = {
    BEValueSerializer.createSerializer[Seq[(BEString, BEValue)]] {
      values =>
        s"${BE.DictionaryStart}${values.map(bevalue => s"${bevalue._1.serialized}${bevalue._2.serialized}").mkString("")}${BE.ValueEnd}"
    }
  }
}


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
      case BE.IntegerStart => decodeInt
      case BE.ListStart => decodeList
      case BE.DictionaryStart => decodeDictionary
      case _ => decodeString
    }
  }

  private def decodeInt: BEInt = {
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

  private def decodeString: BEString = {
    val length = stringLength
    BEString((for(i <- 0 until length) yield next).mkString)
  }

  private def decodeList: BEList = {
    assert(current == BE.ListStart)
    val res = ListBuffer[BEValue]()
    while(next != BE.ValueEnd) {
      res += decode
    }
    assert(current == BE.ValueEnd)
    BEList(res:_*)
  }

  private def decodeDictionary: BEDictionary = {
    assert(current == BE.DictionaryStart)
    val res = ListBuffer[(BEString, BEValue)]()
    next
    while (current != BE.ValueEnd) {
      val key = decodeString
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