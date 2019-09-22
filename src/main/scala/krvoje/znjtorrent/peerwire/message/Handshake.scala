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

import krvoje.znjtorrent.peerwire.value.{PeerID, SHA1}
import shapeless._

case class Handshake(
  infoHash: SHA1,
  peerID: PeerID
) extends PeerwireMessage {

  val choked: Boolean = true
  val interested: Boolean = false

  override val len: Int = PeerwireMessage.PSTR.length.toByte
  private val pstrlen = PeerwireMessage.PSTR.length

  val serialized: Array[Byte] = Array.concat(
    Array[Byte](pstrlen.toByte),
    PeerwireMessage.PSTR.getBytes("UTF-8"),
    PeerwireMessage.RESERVED,
    infoHash.value.getBytes(),
    peerID.value.getBytes()
  )
}

//noinspection ScalaStyle
object Handshake extends Deser[Handshake]{
  val ID: Byte = -1 // Ignore
  def deserialize(bytes: Array[Byte]): Handshake = {
    // TODO: require(bytes.length == bytes.length, "Invalid Handshake length")
    Handshake(
      infoHash = SHA1(bytes(28) :: bytes(29) :: bytes(30) :: bytes(31) :: bytes(32) ::
        bytes(33) :: bytes(34) :: bytes(35) :: bytes(36) :: bytes(37) ::
        bytes(38) :: bytes(39) :: bytes(40) :: bytes(41) :: bytes(42) ::
        bytes(43) :: bytes(44) :: bytes(45) :: bytes(46) :: bytes(47) :: HNil),
      peerID = PeerID(bytes(48) :: bytes(49) :: bytes(50) :: bytes(51) :: bytes(52) ::
        bytes(53) :: bytes(54) :: bytes(55) :: bytes(56) :: bytes(57) ::
        bytes(58) :: bytes(59) :: bytes(60) :: bytes(61) :: bytes(62) ::
        bytes(63) :: bytes(64) :: bytes(65) :: bytes(66) :: bytes(67) :: HNil
      )
    )
  }

  def isHandshake(bs: Array[Byte]): Boolean = {
    val pstrLen = bs(0).asInstanceOf[Int]
    bs.length == pstrLen + 49
  }
}
