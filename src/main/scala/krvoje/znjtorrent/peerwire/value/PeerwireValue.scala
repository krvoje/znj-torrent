package krvoje.znjtorrent.peerwire.value

trait PeerwireValue[T] {
  val value: T
  val serialized: Array[Byte]
}
