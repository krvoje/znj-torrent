package krvoje.znjtorrent.peerwire.message

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