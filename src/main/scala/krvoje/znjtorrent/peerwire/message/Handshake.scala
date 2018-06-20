package krvoje.znjtorrent.peerwire.message

import krvoje.znjtorrent.peerwire.value.{PeerID, SHA1}

case class Handshake(
  infoHash: SHA1,
  peerID: PeerID
) extends PeerwireMessage {

  val choked: Boolean = true
  val interested: Boolean = false

  override val len: Int = PeerwireMessage.PSTR.length.toByte

  val serialized: Array[Byte] = Array.concat(
    Array[Byte](PeerwireMessage.PSTR.length.toByte),
    PeerwireMessage.PSTR,
    PeerwireMessage.RESERVED,
    infoHash.value.getBytes(),
    peerID.value.getBytes()
  )
}

object Handshake extends Deser[Handshake]{
  val ID = -1 // Ignore
  def deserialize(bytes: Array[Byte]): Handshake = {
    //TODO: require(bytes.length == ser.length, "Invalid Handshake length")
    Handshake(
      infoHash = SHA1(new String(bytes.slice(28, 48), "utf-8")),
      peerID = PeerID(new String(bytes.slice(48, bytes.length), "utf-8"))
    )
  }

  def isHandshake(bs: Array[Byte]): Boolean = {
    val pstrLen = bs(0).asInstanceOf[Int]
    bs.length == pstrLen + 49
  }
}