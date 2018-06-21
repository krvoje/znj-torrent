package krvoje.znjtorrent.peerwire.value

import krvoje.znjtorrent.Byte20

case class SHA1(bytes: Byte20) extends PeerwireValue[String] {
  override val serialized: Array[Byte] = bytes.toArray[Byte]
  val value = new String(serialized, "utf-8")
}
