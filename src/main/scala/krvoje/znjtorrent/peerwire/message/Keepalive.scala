package krvoje.znjtorrent.peerwire.message

case class Keepalive() extends PeerwireMessage {
  override val len: Int = 0
  val serialized: Array[Byte] = LEN
}

object Keepalive extends Deser[Keepalive]{
  val ID = -1 // Ignore
  def deserialize(bs: Array[Byte]) = {
    Keepalive()
  }
}