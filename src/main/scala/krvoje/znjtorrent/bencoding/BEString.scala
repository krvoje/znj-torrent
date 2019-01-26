package krvoje.znjtorrent.bencoding

case class BEString(value: String) extends BEValue {
  type T = String
}

object BEString {
  implicit val serializer: BEValueSerializer[BEString] = BEValueSerializer.createSerializer[BEString](bEString =>
    s"${bEString.value.length}${BEValue.StringDelimiter}${bEString.value}")
}