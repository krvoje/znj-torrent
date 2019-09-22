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

package krvoje.znjtorrent.peerwire.value

import krvoje.znjtorrent.PimpedInt

import scala.language.implicitConversions

case class Index(val value: Int) extends PeerwireValue[Int] {

  def <= (other: Index): Boolean = value <= other.value
  def >= (other: Index): Boolean = value >= other.value

  val serialized: Array[Byte] = {
    val res = value.bigEndianByteArray
    val pad = (4 - res.length) max 0
    Array.concat(Array.fill[Byte](pad)(0), res)
  }

  require(serialized.length == 4, s"Invalid index value, too many bytes: $value")
}

object Index {

  implicit def fromInt(value: Int): Index = Index(value)

  def apply(bs: Array[Byte]): Index = {
    this(BigInt(bs).intValue())
  }
}
