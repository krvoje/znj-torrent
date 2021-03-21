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

// TODO: Use a dict instead of Seq of tuples
case class BEDictionary(value: (BEString, BEValue)*) extends BEValue {
  type T = Seq[(BEString, BEValue)]

  val dict: Map[String, BEValue] = value.map(v => v._1.value -> v._2).toMap

  def string(key: String): String = dict(key).asInstanceOf[BEString].value

  def stringOpt(key: String): Option[String] = dict.get(key).map(_.asInstanceOf[BEString].value)

  def int(key: String): Int = dict(key).asInstanceOf[BEInt].value

  def intOpt(key: String): Option[Int] = dict.get(key).map(_.asInstanceOf[BEInt].value)
}

object BEDictionary {
  implicit val serializer: BEValueSerializer[BEDictionary] = {
    BEValueSerializer.createSerializer[BEDictionary] {
      bEDictionary =>
        s"${BEValue.DictionaryStart}${
          bEDictionary.value.map(bevalue => s"${
            BEValueSerializer.serialize(bevalue._1)
          }${
            BEValueSerializer.serialize(bevalue._2)
          }").mkString("")
        }${BEValue.ValueEnd}"
    }
  }
}
