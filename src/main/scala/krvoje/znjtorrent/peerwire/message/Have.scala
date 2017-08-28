package krvoje.znjtorrent.peerwire.message

import krvoje.znjtorrent.peerwire.value.Index

case class Have(index: Index) extends PeerwireMessage {
  override val len: Int = 5
  override val ser: Array[Byte] = Array.concat(
    LEN,
    Array(Have.ID),
    index.ser)
}

object Have extends Deser[Have] {
  override val ID: Byte = 4
  override def deser(bs: Array[Byte]): Have = {
    Have(Index(bs.slice(5,9)))
  }
}