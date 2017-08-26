package krvoje.znjtorrent.peerwire

case class DecimalByte(byte: Byte) {
  require(byte.asInstanceOf[Char] >= '0' && byte.asInstanceOf[Char] <= 9, "Decimal byte required")
}

case class PortValue(index: Int) {
  val value: Array[Byte] = {
    val arr = Peerwire.beba(index)
    val pad = (2 - arr.length) max 0
    Array.concat(Seq.fill[Byte](pad)(0).toArray, arr)
  }

  require(value.length == 2, s"Invalid index value, too many bytes: $index")
}

case class Index(index: Int) {
  val value: Array[Byte] = {
    val res = Peerwire.beba(index)
    val pad = (4 - res.length) max 0
    Array.concat(Array.fill[Byte](pad)(0), res)
  }

  require(value.length == 4, s"Invalid index value, too many bytes: $index")
}

case class SHA1(sha1: String) {
  require(sha1.getBytes().length == 20, "Invalid SHA-2 value (must have 20 bytes)")
}

case class PeerID(peerID: String) {
  require(peerID.getBytes().length == 20, "Invalid peer_id value (must have 20 bytes)")
}

trait PeerwireMessage {
  val ID: Byte = -1
  val len: Int
  require(LEN.length == 4, s"Message too long $len")


  def LEN: Array[Byte] = {
    val arr = Peerwire.beba(len)
    val pad = Array.fill[Byte]((4 - arr.length) max 0)(0)
    Array.concat(pad, arr)
  }

  def ser: Array[Byte]
}

case class Handshake(
  infoHash: SHA1,
  peerID: PeerID
) extends PeerwireMessage {

  val choked: Boolean = true
  val interested: Boolean = false

  override val len: Int = Peerwire.PSTR.length.toByte

  val ser: Array[Byte] = Array.concat(
    Array[Byte](Peerwire.PSTR.length.toByte),
    Peerwire.PSTR,
    Peerwire.RESERVED,
    infoHash.sha1.getBytes(),
    peerID.peerID.getBytes()
  )

  def deserialize(bytes: Array[Byte]): Handshake = {
    require(bytes.length == ser.length, "Invalid Handshake length")

    Handshake(
      infoHash = SHA1(new String(bytes.slice(28, 48), "utf-8")),
      peerID = PeerID(new String(bytes.slice(48, bytes.length), "utf-8"))
    )
  }
}

case class Keepalive() extends PeerwireMessage {
  override val len: Int = 0
  val ser: Array[Byte] = LEN
}

case class Choke() extends PeerwireMessage {
  override val len: Int = 1
  override val ID: Byte = 0
  override val ser: Array[Byte] = Array.concat(
    LEN,
    Array(ID))
}

case class Unchoke() extends PeerwireMessage {
  override val len: Int = 1
  override val ID: Byte = 1
  override val ser: Array[Byte] = Array.concat(
    LEN,
    Array(ID))
}

case class Interested() extends PeerwireMessage {
  override val len: Int = 1
  override val ID: Byte = 2
  override val ser: Array[Byte] = Array.concat(
    LEN,
    Array(ID))
}

case class NotInterested() extends PeerwireMessage {
  override val len: Int = 1
  override val ID: Byte = 3
  override val ser: Array[Byte] = Array.concat(
    LEN,
    Array(ID))
}

case class Have(index: Index) extends PeerwireMessage {
  override val len: Int = 5
  override val ID: Byte = 4
  override val ser: Array[Byte] = Array.concat(
    LEN,
    Array(ID),
    index.value)
}

case class Bitfield(payload: Array[Boolean]) extends PeerwireMessage {

  private val BYTE_SIZE = 8

  private def pad(bs: Array[Boolean]): Array[Boolean] = {
    val leftToPad = if(bs.length % BYTE_SIZE != 0) (BYTE_SIZE - bs.length % BYTE_SIZE) else 0
    (bs.toSeq ++ (for(i <- 0 until leftToPad) yield false))
      .toArray
  }

  val value: Array[Byte] = {
    pad(payload)
      .grouped(BYTE_SIZE)
      .map { bs =>
        assert(bs.length == BYTE_SIZE)
        var res: Int = 0
        for(i <- 1 to BYTE_SIZE) {
          if(bs(i-1))
            res = res | (1 << (BYTE_SIZE - i))
        }
        res.asInstanceOf[Byte]
      }
      .toArray
  }

  override val len: Int = (1 + value.length)
  override val ID: Byte = 5
  override val ser: Array[Byte] = Array.concat(
    LEN,
    Array(ID),
    value)
}

case class Request(index: Index, begin: Index, end: Index) extends PeerwireMessage {
  override val len: Int = 13
  override val ID: Byte = 6
  override val ser: Array[Byte] = Array.concat(
    LEN,
    Array(ID),
    index.value,
    begin.value,
    end.value)
}

case class Piece(index: Index, begin: Index, block: Array[Byte]) extends PeerwireMessage {
  override val len: Int = 9 + block.length
  override val ID: Byte = 7
  override val ser: Array[Byte] =  Array.concat(
    LEN,
    Array(ID),
    index.value,
    begin.value,
    block)
}

case class Cancel(index: Index, begin: Index, end: Index) extends PeerwireMessage {
  override val len: Int = 13
  override val ID: Byte = 8
  override val ser: Array[Byte] =  Array.concat(
    LEN,
    Array(ID),
    index.value,
    begin.value,
    end.value)
}

case class Port(port: PortValue) extends PeerwireMessage {
  override val len: Int = 3
  override val ID: Byte = 9
  override val ser: Array[Byte] =  Array.concat(
    LEN,
    Array(ID),
    port.value)
}

object Peerwire {
  val PSTR: Array[Byte] = "BitTorrent protocol".getBytes("UTF-8")
  val RESERVED = Array[Byte](0,0,0,0,0,0,0,0)
  def beba(index: Int): Array[Byte] = {
    BigInt(index).toByteArray
  }
}