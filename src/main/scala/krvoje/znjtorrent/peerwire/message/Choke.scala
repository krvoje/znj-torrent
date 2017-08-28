package krvoje.znjtorrent.peerwire.message

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