package krvoje.znjtorrent

package object peerwire {
  implicit class RichInt(val value: Int) extends AnyVal {
    def bigEndianByteArray: Array[Byte] = BigInt(value).toByteArray
  }
}
