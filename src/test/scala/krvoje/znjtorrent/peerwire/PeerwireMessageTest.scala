package krvoje.znjtorrent.peerwire

import krvoje.znjtorrent.Byte20
import krvoje.znjtorrent.peerwire.message._
import krvoje.znjtorrent.peerwire.value.{Index, PeerID, PortValue, SHA1}
import org.specs2._
import shapeless._

class PeerwireMessageTest extends mutable.Specification {

  "Serialization roundtrip" >> {
    roundtrip(Handshake(SHA1("0123456789ABCDEFGHIJ"), PeerID("0123456789ABCDEFGHIJ")))
    roundtrip(Keepalive())
    roundtrip(Choke())
    roundtrip(Unchoke())
    roundtrip(Interested())
    roundtrip(NotInterested())
    roundtrip(Have(Index(10)))
    roundtrip(Bitfield(false, false, false))
    roundtrip(Request(10, 2, 11)) // TODO: Invalid
    roundtrip(Piece(10, 2, Array[Byte](1, 1))) // TODO: Invalid
    roundtrip(Cancel(10, 2, 11)) // TODO: Invalid
    roundtrip(Port(PortValue(9000)))
  }

  private def roundtrip(msg: PeerwireMessage) = {
    s"$msg" >> {
      "Serialize -> Deserialize" >> { PeerwireMessage.deserialize(msg.serialized) ==== padBools(msg) }
      "Ser - > Deser -> Ser" >> {PeerwireMessage.deserialize(msg.serialized).serialized ==== msg.serialized}
    }
  }

  private def padBools(msg: PeerwireMessage): PeerwireMessage = msg match {
    case Bitfield(pl) => Bitfield(padBools(pl))
    case _ => msg
  }

  private def padBools(bs: Array[Boolean]): Array[Boolean] = {
    val toPad = (bs.length % Bitfield.BYTE_SIZE - Bitfield.BYTE_SIZE).abs
    bs.padTo(bs.length + toPad, false)
  }

  implicit def str2b20(str: String): Byte20 = {
    val bytes = str.getBytes("utf-8")
    bytes(0) :: bytes(1) :: bytes(2) :: bytes(3) :: bytes(4) :: bytes(5) :: bytes(6) :: bytes(7) :: bytes(8) :: bytes(9) ::
      bytes(10) :: bytes(11) :: bytes(12) :: bytes(13) :: bytes(14) :: bytes(15) :: bytes(16) :: bytes(17) :: bytes(18) :: bytes(19) :: HNil
  }
}
