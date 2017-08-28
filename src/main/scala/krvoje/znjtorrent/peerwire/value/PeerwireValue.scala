package krvoje.znjtorrent.peerwire.value

trait PeerwireValue[T] {
  val value: T
  val ser: Array[Byte]
}
