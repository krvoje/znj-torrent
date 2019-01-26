package krvoje.znjtorrent.bencoding

trait BEValue {
  type T
  val value: T
}

object BEValue {
  val StringDelimiter = ':'
  val IntegerStart = 'i'
  val ListStart = 'l'
  val DictionaryStart = 'd'
  val ValueEnd = 'e'
}