package krvoje.znjtorrent.peerwire.value

case class PeerID(val value: String) extends PeerwireValue[String] {
  require(value.getBytes().length == 20, "Invalid peer_id value (must have 20 bytes)")
  override val ser: Array[Byte] = value.getBytes
}

















































