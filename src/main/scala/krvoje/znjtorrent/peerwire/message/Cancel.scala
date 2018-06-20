package krvoje.znjtorrent.peerwire.message

import krvoje.znjtorrent.peerwire.value.Index

case class Cancel(index: Index, begin: Index, end: Index) extends PeerwireMessage {

  require(index >= begin && index <= end, s"Invalid params: $this")
  require(begin <= end, s"Invalid params: $this")

  override val len: Int = 13
  override val serialized: Array[Byte] =  Array.concat(
    LEN,
    Array(Cancel.ID),
    index.ser,
    begin.ser,
    end.ser)
}

object Cancel extends Deser[Cancel] {
  override val ID: Byte = 8
  override def deserialize(bs: Array[Byte]) = {
    Cancel(
      index = Index(bs.slice(5, 9)),
      begin = Index(bs.slice(9, 13)),
      end = Index(bs.slice(13, 17)))
  }

  def apply(index: Int, begin: Int, end: Int): Cancel =
    Cancel(Index(index), Index(begin), Index(end))
}