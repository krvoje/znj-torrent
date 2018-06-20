package krvoje.znjtorrent.peerwire.message

import scala.collection.mutable.ArrayBuffer

case class Bitfield(payload: Array[Boolean]) extends PeerwireMessage {

  private def pad(bs: Array[Boolean]): Array[Boolean] = {
    val leftToPad = if(bs.length % Bitfield.BYTE_SIZE != 0) (Bitfield.BYTE_SIZE - bs.length % Bitfield.BYTE_SIZE) else 0
    (bs.toSeq ++ (for(i <- 0 until leftToPad) yield false))
      .toArray
  }

  val value: Array[Byte] = {
    pad(payload)
      .grouped(Bitfield.BYTE_SIZE)
      .map { bs =>
        assert(bs.length == Bitfield.BYTE_SIZE)
        var res: Int = 0
        for(i <- 0 until Bitfield.BYTE_SIZE) {
          if(bs(i))
            res = res | (1 << (Bitfield.BYTE_SIZE - i - 1))
        }
        res.asInstanceOf[Byte]
      }
      .toArray
  }

  override val len: Int = (1 + value.length)
  override val serialized: Array[Byte] = Array.concat(
    LEN,
    Array(Bitfield.ID),
    value)

  override def equals(that: Any): Boolean = {
    that match {
      case that@Bitfield(pl) =>
        println(s"${payload.toSeq} vs ${that.payload.toSeq}")
        payload.deep == that.payload.deep
      case _ => false
    }
  }
}

object Bitfield extends Deser[Bitfield] {
  override val ID: Byte = 5
  val BYTE_SIZE = 8

  def apply(bs: Boolean*): Bitfield = Bitfield(bs.toArray)

  override def deserialize(bs: Array[Byte]): Bitfield = {
    val valueLen = BigInt(bs.slice(0,4))-1
    Bitfield(toBools(bs.slice(5, 5 + valueLen.intValue())))
  }

  private def toBools(bs: Array[Byte]): Array[Boolean] = {
    var res = ArrayBuffer[Boolean]()
    for {
      b <- bs
      i <- (BYTE_SIZE - 1).to(0, -1)
    } {
      val bool = (b & (1 << i)) != 0
      res.append(bool)
    }
    res.toArray
  }
}