package krvoje.znjtorrent.peerwire.value

case class SHA1(val value: String) extends PeerwireValue[String] {
  require(value.getBytes().length == 20, "Invalid SHA-2 value (must have 20 bytes)")

  override val ser: Array[Byte] = value.getBytes
}
