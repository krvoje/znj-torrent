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

case class Cancel(index: Index, begin: Index, end: Index) extends PeerwireMessage {

  require(index >= begin && index <= end, s"Invalid params: $this")
  require(begin <= end, s"Invalid params: $this")

  override val len       : Int         = 13
  override val serialized: Array[Byte] = Array.concat(
    LEN,
    Array(Cancel.ID),
    index.serialized,
    begin.serialized,
    end.serialized)
}

object Cancel extends Deser[Cancel] {
  override val ID: Byte = 8

  override def deserialize(bs: Array[Byte]): Cancel = {
    Cancel(
      index = Index(bs.slice(5, 9)),
      begin = Index(bs.slice(9, 13)),
      end = Index(bs.slice(13, 17)))
  }

  def apply(index: Int, begin: Int, end: Int): Cancel =
    Cancel(Index(index), Index(begin), Index(end))
}
