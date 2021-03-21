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

package krvoje.znjtorrent.bencoding

trait BEValueSerializer[T] {
  def serialize(value: T): String
}

object BEValueSerializer {
  def apply[T](implicit enc: BEValueSerializer[T]): BEValueSerializer[T] = enc

  def createSerializer[T](f: T => String): BEValueSerializer[T] = (value: T) => f(value)

  def serialize(value: BEValue)(
    implicit
    strS : BEValueSerializer[BEString],
    intS : BEValueSerializer[BEInt],
    listS: BEValueSerializer[BEList],
    dictS: BEValueSerializer[BEDictionary],
  ): String = value match {
    case beString: BEString         => strS.serialize(beString)
    case beInt: BEInt               => intS.serialize(beInt)
    case beList: BEList             => listS.serialize(beList)
    case bEDictionary: BEDictionary => dictS.serialize(bEDictionary)
  }
}
