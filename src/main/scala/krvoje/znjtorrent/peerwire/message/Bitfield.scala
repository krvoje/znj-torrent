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

import scala.collection.mutable.ArrayBuffer

case class Bitfield(payload: Array[Boolean]) extends PeerwireMessage {

  override val len       : Int         = (1 + value.length)
  override val serialized: Array[Byte] = Array.concat(
    LEN,
    Array(Bitfield.ID),
    value)
  lazy val value: Array[Byte] = {
    pad(payload)
      .grouped(Bitfield.BYTE_SIZE)
      .map { bs =>
        assert(bs.length == Bitfield.BYTE_SIZE)
        var res: Int = 0
        for (i <- 0 until Bitfield.BYTE_SIZE) {
          if (bs(i))
            res = res | (1 << (Bitfield.BYTE_SIZE - i - 1))
        }
        res.asInstanceOf[Byte]
      }
      .toArray
  }

  override def equals(that: Any): Boolean = {
    that match {
      case that@Bitfield(pl) =>
        println(s"${payload.toSeq} vs ${that.payload.toSeq}")
        payload.sameElements(that.payload)
      case _                 => false
    }
  }

  override def hashCode(): Int = payload.hashCode()

  private def pad(bs: Array[Boolean]): Array[Boolean] = {
    val leftToPad = if (bs.length % Bitfield.BYTE_SIZE != 0) (Bitfield.BYTE_SIZE - bs.length % Bitfield.BYTE_SIZE) else 0
    (bs.toSeq ++ (for (i <- 0 until leftToPad) yield false))
      .toArray
  }
}

object Bitfield extends Deser[Bitfield] {
  override val ID: Byte = 5
  val BYTE_SIZE = 8

  def apply(bs: Boolean*): Bitfield = Bitfield(bs.toArray)

  override def deserialize(bs: Array[Byte]): Bitfield = {
    val valueLen = BigInt(bs.slice(0, 4)) - 1
    Bitfield(toBools(bs.slice(5, 5 + valueLen.intValue)))
  }

  private def toBools(bs: Array[Byte]): Array[Boolean] = {
    val res = ArrayBuffer[Boolean]()
    for {
      b <- bs
      i <- (BYTE_SIZE - 1).to(0, -1)
    } {
      val bool = (b & (1 << i)) != 0
      res.append(bool)
    }
    res.toArray
  }
}
