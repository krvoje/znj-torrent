package krvoje
import shapeless._

package object znjtorrent {

  type Byte20 = Byte :: Byte ::Byte :: Byte :: Byte :: Byte :: Byte :: Byte :: Byte :: Byte ::
    Byte :: Byte ::Byte :: Byte :: Byte :: Byte :: Byte :: Byte :: Byte :: Byte :: HNil

  implicit class PimpedInt(val value: Int) extends AnyVal {
    def bigEndianByteArray: Array[Byte] = BigInt(value).toByteArray
  }
}
