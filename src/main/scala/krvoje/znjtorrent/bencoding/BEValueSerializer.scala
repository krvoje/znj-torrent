package krvoje.znjtorrent.bencoding

trait BEValueSerializer[T] {
  def serialize(value: T): String
}

object BEValueSerializer {
  def apply[T](implicit enc: BEValueSerializer[T]): BEValueSerializer[T] = enc
  def createSerializer[T](f: T => String) = new BEValueSerializer[T] {
    override def serialize(value: T): String = f(value)
  }

  def serialize(value: BEValue)(
    implicit
    strS: BEValueSerializer[BEString],
    intS: BEValueSerializer[BEInt],
    listS: BEValueSerializer[BEList],
    dictS: BEValueSerializer[BEDictionary],
  ) = value match {
    case beString: BEString => strS.serialize(beString)
    case beInt: BEInt => intS.serialize(beInt)
    case beList: BEList => listS.serialize(beList)
    case bEDictionary: BEDictionary => dictS.serialize(bEDictionary)
  }
}