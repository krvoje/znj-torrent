package krvoje.znjtorrent.peerwire.value

import krvoje.znjtorrent.PimpedInt


case class PortValue(val value: Int) extends PeerwireValue[Int] {

  override val serialized: Array[Byte] = {
    val arr = value.bigEndianByteArray
    val pad = (2 - arr.length) max 0
    Array.concat(Seq.fill[Byte](pad)(0).toArray, arr)
  }

  require(serialized.length == 2, s"Invalid index value, too many bytes: $value")
}

object PortValue {
  def apply(bs: Array[Byte]): PortValue = {
    require(bs.length == 2, s"Invalid port byte value: $bs")
    PortValue(BigInt(bs).intValue)
  }
}