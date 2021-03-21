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

import krvoje.znjtorrent.PimpedInt

object PeerwireMessage {
  val PSTR                  = "BitTorrent protocol"
  val RESERVED: Array[Byte] = Array[Byte](0, 0, 0, 0, 0, 0, 0, 0)

  def deserialize(bs: Array[Byte]): PeerwireMessage = {
    require(bs.length >= 4, "Invalid payload, less than 4 bytes.")
    val len = BigInt(bs.slice(0, 4))
    if (Handshake.isHandshake(bs)) {
      Handshake.deserialize(bs)
    } else if (len == 0) {
      Keepalive.deserialize(bs)
    } else {
      val ID = bs(4)
      ID match {
        case Choke.ID         => Choke.deserialize(bs)
        case Unchoke.ID       => Unchoke.deserialize(bs)
        case Interested.ID    => Interested.deserialize(bs)
        case NotInterested.ID => NotInterested.deserialize(bs)
        case Have.ID          => Have.deserialize(bs)
        case Bitfield.ID      => Bitfield.deserialize(bs)
        case Request.ID       => Request.deserialize(bs)
        case Piece.ID         => Piece.deserialize(bs)
        case Cancel.ID        => Cancel.deserialize(bs)
        case Port.ID          => Port.deserialize(bs)
        case _                => throw new RuntimeException(s"Invalid messsage ID: $ID")
      }
    }
  }
}

trait PeerwireMessage {
  val len: Int
  require(LEN.length == 4, s"Message too long $len")


  def LEN: Array[Byte] = {
    val arr = len.bigEndianByteArray
    val pad = Array.fill[Byte]((4 - arr.length) max 0)(0)
    Array.concat(pad, arr)
  }

  def serialized: Array[Byte]
}

trait Deser[MESSAGE_TYPE <: PeerwireMessage] {
  val ID: Byte

  def deserialize(bs: Array[Byte]): MESSAGE_TYPE
}
