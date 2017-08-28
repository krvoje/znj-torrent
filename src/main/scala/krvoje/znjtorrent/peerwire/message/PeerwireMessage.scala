package krvoje.znjtorrent.peerwire.message

import krvoje.znjtorrent.peerwire._

object PeerwireMessage {
  val PSTR: Array[Byte] = "BitTorrent protocol".getBytes("UTF-8")
  val RESERVED = Array[Byte](0,0,0,0,0,0,0,0)
  def beba(index: Int): Array[Byte] = {
    BigInt(index).toByteArray
  }

  def deserialize(bs: Array[Byte]): PeerwireMessage = {
    require(bs.length >= 4, "Invalid payload, less than 4 bytes.")
    val len = BigInt(bs.slice(0, 4))
    if(Handshake.isHandshake(bs)) {
      Handshake.deser(bs)
    } else if(len == 0) {
      Keepalive.deser(bs)
    } else {
      val ID = bs(4)
      ID match {
        case Choke.ID => Choke.deser(bs)
        case Unchoke.ID => Unchoke.deser(bs)
        case Interested.ID => Interested.deser(bs)
        case NotInterested.ID => NotInterested.deser(bs)
        case Have.ID => Have.deser(bs)
        case Bitfield.ID => Bitfield.deser(bs)
        case Request.ID => Request.deser(bs)
        case Piece.ID => Piece.deser(bs)
        case Cancel.ID => Cancel.deser(bs)
        case Port.ID => Port.deser(bs)
        case _ => throw new RuntimeException(s"Invalid messsage ID: $ID")
      }
    }
  }
}

trait PeerwireMessage {
  val len: Int
  require(LEN.length == 4, s"Message too long $len")


  def LEN: Array[Byte] = {
    val arr = PeerwireMessage.beba(len)
    val pad = Array.fill[Byte]((4 - arr.length) max 0)(0)
    Array.concat(pad, arr)
  }

  def ser: Array[Byte]
}

trait Deser[MESSAGE_TYPE <: PeerwireMessage] {
  val ID: Byte
  def deser(bs: Array[Byte]): MESSAGE_TYPE
}