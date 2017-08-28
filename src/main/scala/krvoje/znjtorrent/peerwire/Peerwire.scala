package krvoje.znjtorrent.peerwire

import scala.collection.mutable.ArrayBuffer

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

object PortValue {
  def apply(bs: Array[Byte]): PortValue = {
    require(bs.length == 2, s"Invalid port byte value: $bs")
    PortValue(BigInt(bs).intValue)
  }
}

case class Index(index: Int) {

  def <= (other: Index): Boolean = index <= other.index
  def >= (other: Index): Boolean = index >= other.index

  val value: Array[Byte] = {
    val res = Peerwire.beba(index)
    val pad = (4 - res.length) max 0
    Array.concat(Array.fill[Byte](pad)(0), res)
  }

  require(value.length == 4, s"Invalid index value, too many bytes: $index")
}

object Index {
  def apply(bs: Array[Byte]): Index = {
    this(BigInt(bs).intValue())
  }
}

case class SHA1(sha1: String) {
  require(sha1.getBytes().length == 20, "Invalid SHA-2 value (must have 20 bytes)")
}

case class PeerID(peerID: String) {
  require(peerID.getBytes().length == 20, "Invalid peer_id value (must have 20 bytes)")
}

trait PeerwireMessage {
  val len: Int
  require(LEN.length == 4, s"Message too long $len")


  def LEN: Array[Byte] = {
    val arr = Peerwire.beba(len)
    val pad = Array.fill[Byte]((4 - arr.length) max 0)(0)
    Array.concat(pad, arr)
  }

  def ser: Array[Byte]
}

trait Deser[MESSAGE_TYPE <: PeerwireMessage] {
  val ID: Byte
  def deser(bs: Array[Byte]): MESSAGE_TYPE
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
}

object Handshake extends Deser[Handshake]{
  val ID = -1 // Ignore
  def deser(bytes: Array[Byte]): Handshake = {
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

case class Keepalive() extends PeerwireMessage {
  override val len: Int = 0
  val ser: Array[Byte] = LEN
}

object Keepalive extends Deser[Keepalive]{
  val ID = -1 // Ignore
  def deser(bs: Array[Byte]) = {
    Keepalive()
  }
}

case class Choke() extends PeerwireMessage {
  override val len: Int = 1
  override val ser: Array[Byte] = Array.concat(
    LEN,
    Array(Choke.ID))
}

object Choke extends Deser[Choke] {
  override val ID: Byte = 0
  def deser(bs: Array[Byte]) = {
    Choke()
  }
}

case class Unchoke() extends PeerwireMessage {
  override val len: Int = 1
  override val ser: Array[Byte] = Array.concat(
    LEN,
    Array(Unchoke.ID))
}

object Unchoke extends Deser[Unchoke] {
  override val ID: Byte = 1
  override def deser(bs: Array[Byte]): Unchoke = {
    Unchoke()
  }
}

case class Interested() extends PeerwireMessage {
  override val len: Int = 1
  override val ser: Array[Byte] = Array.concat(
    LEN,
    Array(Interested.ID))
}

object Interested extends Deser[Interested] {
  override val ID: Byte = 2
  override def deser(bs: Array[Byte]): Interested = {
    Interested()
  }
}

case class NotInterested() extends PeerwireMessage {
  override val len: Int = 1
  override val ser: Array[Byte] = Array.concat(
    LEN,
    Array(NotInterested.ID))
}

object NotInterested extends Deser[NotInterested] {
  override val ID: Byte = 3
  override def deser(bs: Array[Byte]): NotInterested = {
    NotInterested()
  }
}

case class Have(index: Index) extends PeerwireMessage {
  override val len: Int = 5
  override val ser: Array[Byte] = Array.concat(
    LEN,
    Array(Have.ID),
    index.value)
}

object Have extends Deser[Have] {
  override val ID: Byte = 4
  override def deser(bs: Array[Byte]): Have = {
    Have(Index(bs.slice(5,9)))
  }
}

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
  override val ser: Array[Byte] = Array.concat(
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

  override def deser(bs: Array[Byte]): Bitfield = {
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

case class Request(index: Index, begin: Index, end: Index) extends PeerwireMessage {

  require(index >= begin && index <= end, s"Invalid params: $this")
  require(begin <= end, s"Invalid params: $this")

  override val len: Int = 13
  override val ser: Array[Byte] = Array.concat(
    LEN,
    Array(Request.ID),
    index.value,
    begin.value,
    end.value)
}

object Request extends Deser[Request] {
  override val ID: Byte = 6
  override def deser(bs: Array[Byte]) = {
    Request(
      index = Index(bs.slice(5, 9)),
      begin = Index(bs.slice(9, 13)),
      end = Index(bs.slice(13, 17)))
  }

  def apply(index: Int, begin: Int, end: Int): Request =
    Request(Index(index), Index(begin), Index(end))
}

case class Piece(index: Index, begin: Index, block: Array[Byte]) extends PeerwireMessage {
  override val len: Int = 9 + block.length
  override val ser: Array[Byte] =  Array.concat(
    LEN,
    Array(Piece.ID),
    index.value,
    begin.value,
    block)

  override def equals(that: Any): Boolean = {
    that match {
      case that@Piece(_,_,_) =>
        index == that.index &&
        begin == that.begin &&
        block.deep == that.block.deep
      case _ => false
    }
  }
}

object Piece extends Deser[Piece] {
  override val ID: Byte = 7
  override def deser(bs: Array[Byte]) = {
    val blockLength = BigInt(bs.slice(0, 4)).intValue() - 9
    Piece(
      index = Index(bs.slice(5, 9)),
      begin = Index(bs.slice(9, 13)),
      block = bs.slice(13, 13 + blockLength))
  }
  def apply(index: Int, begin: Int, block: Array[Byte]): Piece =
    Piece(Index(index), Index(begin), block)
}

case class Cancel(index: Index, begin: Index, end: Index) extends PeerwireMessage {

  require(index >= begin && index <= end, s"Invalid params: $this")
  require(begin <= end, s"Invalid params: $this")

  override val len: Int = 13
  override val ser: Array[Byte] =  Array.concat(
    LEN,
    Array(Cancel.ID),
    index.value,
    begin.value,
    end.value)
}

object Cancel extends Deser[Cancel] {
  override val ID: Byte = 8
  override def deser(bs: Array[Byte]) = {
    Cancel(
      index = Index(bs.slice(5, 9)),
      begin = Index(bs.slice(9, 13)),
      end = Index(bs.slice(13, 17)))
  }

  def apply(index: Int, begin: Int, end: Int): Cancel =
    Cancel(Index(index), Index(begin), Index(end))
}

case class Port(port: PortValue) extends PeerwireMessage {
  override val len: Int = 3
  override val ser: Array[Byte] =  Array.concat(
    LEN,
    Array(Port.ID),
    port.value)
}

object Port extends Deser[Port] {
  override val ID: Byte = 9
  override def deser(bs: Array[Byte]) = {
    Port(PortValue(bs.slice(5,7)))
  }

  def apply(portValue: Int): Port = Port(PortValue(portValue))
}

object Peerwire {
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