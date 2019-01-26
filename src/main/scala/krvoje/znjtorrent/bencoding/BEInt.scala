package krvoje.znjtorrent.bencoding

case class BEInt(value: Int) extends BEValue {
  type T = Int
}

object BEInt {
  implicit val serializer: BEValueSerializer[BEInt] = BEValueSerializer.createSerializer[BEInt](beInt =>
    s"${BEValue.IntegerStart}${beInt.value}${BEValue.ValueEnd}")
}