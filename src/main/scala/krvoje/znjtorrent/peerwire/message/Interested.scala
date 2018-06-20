package krvoje.znjtorrent.peerwire.message

object Interested extends Deser[Interested] {
  override val ID: Byte = 2
  override def deserialize(bs: Array[Byte]): Interested = {
    Interested()
  }
}

case class Interested() extends PeerwireMessage {
  override val len: Int = 1
  override val serialized: Array[Byte] = Array.concat(
    LEN,
    Array(Interested.ID))
}