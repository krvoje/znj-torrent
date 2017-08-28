package krvoje.znjtorrent.peerwire

import krvoje.znjtorrent.peerwire.message._
import krvoje.znjtorrent.peerwire.value.{Index, PeerID, PortValue, SHA1}
import org.specs2._

class PeerwireMessageTest extends mutable.Specification {

  "Serialization roundtrip" >> {
    rt(Handshake(SHA1("0123456789ABCDEFGHIJ"), PeerID("0123456789ABCDEFGHIJ")))
    rt(Keepalive())
    rt(Choke())
    rt(Unchoke())
    rt(Interested())
    rt(NotInterested())
    rt(Have(Index(10)))
    rt(Bitfield(false, false, false))
    rt(Request(10, 2, 11)) // TODO: Invalid
    rt(Piece(10, 2, Array[Byte](1, 1))) // TODO: Invalid
    rt(Cancel(10, 2, 11)) // TODO: Invalid
    rt(Port(PortValue(9000)))
  }

  private def rt(msg: PeerwireMessage) = {
    s"$msg" >> {
      "Serialize -> Deserialize" >> { PeerwireMessage.deserialize(msg.ser) ==== padBools(msg) }
      "Ser - > Deser -> Ser" >> {PeerwireMessage.deserialize(msg.ser).ser ==== msg.ser}
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

}
