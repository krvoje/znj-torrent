package krvoje.znjtorrent.bencoding

// TODO: Use a dict instead of Seq of tuples
case class BEDictionary(value: (BEString, BEValue)*) extends BEValue {
  type T = Seq[(BEString, BEValue)]

  val dict: Map[String, BEValue] = value.map(v => v._1.value -> v._2).toMap

  def string(key: String): String = dict(key).asInstanceOf[BEString].value
  def stringOpt(key: String): Option[String] = dict.get(key).map(_.asInstanceOf[BEString].value)
  def int(key: String): Int = dict(key).asInstanceOf[BEInt].value
  def intOpt(key: String): Option[Int] = dict.get(key).map(_.asInstanceOf[BEInt].value)
}

object BEDictionary {
  implicit val serializer: BEValueSerializer[BEDictionary] = {
    BEValueSerializer.createSerializer[BEDictionary] {
      bEDictionary =>
        s"${BEValue.DictionaryStart}${bEDictionary.value.map(bevalue => s"${
          BEValueSerializer.serialize(bevalue._1)
        }${
          BEValueSerializer.serialize(bevalue._2)
        }").mkString("")}${BEValue.ValueEnd}"
    }
  }
}