/*
 * MIT License
 *
 * Copyright (c) 2019 Hrvoje Peradin
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */

package krvoje.znjtorrent.peerwire.message

import krvoje.znjtorrent.peerwire.value.Index

case class Piece(index: Index, begin: Index, block: Array[Byte]) extends PeerwireMessage {
  override val len       : Int         = 9 + block.length
  override val serialized: Array[Byte] = Array.concat(
    LEN,
    Array(Piece.ID),
    index.serialized,
    begin.serialized,
    block)

  override def equals(that: Any): Boolean = {
    that match {
      case that@Piece(_, _, _) =>
        index == that.index &&
          begin == that.begin &&
          block.sameElements(that.block)
      case _                   => false
    }
  }
}

//noinspection ScalaStyle
object Piece extends Deser[Piece] {
  override val ID: Byte = 7

  override def deserialize(bs: Array[Byte]): Piece = {
    val blockLength = BigInt(bs.slice(0, 4)).intValue - 9
    Piece(
      index = Index(bs.slice(5, 9)),
      begin = Index(bs.slice(9, 13)),
      block = bs.slice(13, 13 + blockLength))
  }

  def apply(index: Int, begin: Int, block: Array[Byte]): Piece =
    Piece(Index(index), Index(begin), block)
}
