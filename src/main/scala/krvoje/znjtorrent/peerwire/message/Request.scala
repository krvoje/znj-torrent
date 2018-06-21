package krvoje.znjtorrent.peerwire.message

import krvoje.znjtorrent.peerwire.value.Index

case class Request(index: Index, begin: Index, end: Index) extends PeerwireMessage {

  require(index >= begin && index <= end, s"Invalid params: $this")
  require(begin <= end, s"Invalid params: $this")

  override val len: Int = 13
  override val serialized: Array[Byte] = Array.concat(
    LEN,
    Array(Request.ID),
    index.serialized,
    begin.serialized,
    end.serialized)
}

object Request extends Deser[Request] {
  override val ID: Byte = 6
  override def deserialize(bs: Array[Byte]) = {
    Request(
      index = Index(bs.slice(5, 9)),
      begin = Index(bs.slice(9, 13)),
      end = Index(bs.slice(13, 17)))
  }

  def apply(index: Int, begin: Int, end: Int): Request =
    Request(Index(index), Index(begin), Index(end))
}