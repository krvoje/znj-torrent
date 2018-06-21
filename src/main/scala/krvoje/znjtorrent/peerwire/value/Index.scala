package krvoje.znjtorrent.peerwire.value

import krvoje.znjtorrent.PimpedInt

case class Index(val value: Int) extends PeerwireValue[Int] {

  def <= (other: Index): Boolean = value <= other.value
  def >= (other: Index): Boolean = value >= other.value

  val serialized: Array[Byte] = {
    val res = value.bigEndianByteArray
    val pad = (4 - res.length) max 0
    Array.concat(Array.fill[Byte](pad)(0), res)
  }

  require(serialized.length == 4, s"Invalid index value, too many bytes: $value")
}

object Index {
  def apply(bs: Array[Byte]): Index = {
    this(BigInt(bs).intValue())
  }
}