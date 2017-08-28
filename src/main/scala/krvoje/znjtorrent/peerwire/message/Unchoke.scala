package krvoje.znjtorrent.peerwire.message

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