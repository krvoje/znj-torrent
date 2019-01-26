package krvoje.znjtorrent.bencoding

case class BEList(value: BEValue*) extends BEValue {
  type T = Seq[BEValue]
}

object BEList {
  implicit val serializer: BEValueSerializer[BEList] = {
    BEValueSerializer.createSerializer[BEList]{
      beList => s"${BEValue.ListStart}${beList.value.map(
        BEValueSerializer.serialize
      ).mkString("")}${BEValue.ValueEnd}"
    }
  }
}