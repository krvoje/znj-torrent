package krvoje.znjtorrent.peerwire.message

import krvoje.znjtorrent.peerwire.value.Index

case class Piece(index: Index, begin: Index, block: Array[Byte]) extends PeerwireMessage {
  override val len: Int = 9 + block.length
  override val serialized: Array[Byte] =  Array.concat(
    LEN,
    Array(Piece.ID),
    index.serialized,
    begin.serialized,
    block)

  override def equals(that: Any): Boolean = {
    that match {
      case that@Piece(_,_,_) =>
        index == that.index &&
        begin == that.begin &&
        block.deep == that.block.deep
      case _ => false
    }
  }
}

object Piece extends Deser[Piece] {
  override val ID: Byte = 7
  override def deserialize(bs: Array[Byte]) = {
    val blockLength = BigInt(bs.slice(0, 4)).intValue() - 9
    Piece(
      index = Index(bs.slice(5, 9)),
      begin = Index(bs.slice(9, 13)),
      block = bs.slice(13, 13 + blockLength))
  }
  def apply(index: Int, begin: Int, block: Array[Byte]): Piece =
    Piece(Index(index), Index(begin), block)
}