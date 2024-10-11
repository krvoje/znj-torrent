package krvoje.znjtorrent.peerwire

import krvoje.znjtorrent.peerwire.message.Bitfield
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BitfieldTest extends AnyFlatSpec with Matchers {

  var count = 0

  implicit def stringToBools(str: String): Array[Boolean] = str.toCharArray.flatMap {
    case '0' => Some(false)
    case '1' => Some(true)
    case _   => None
  }

  implicit def int2Byte(int: Int): Byte = int.asInstanceOf[Byte]

  def testBitfield(payload: String, expected: Seq[Byte]): Unit = {
    val actual = Bitfield(payload).value.toSeq
    count += 1
    val title = s"$count. ${payload.grouped(8).mkString(" ")}: $actual vs $expected"
    it should title in {
      actual shouldEqual expected
    }
  }

  testBitfield("0", Seq[Byte](0))
  testBitfield("00", Seq[Byte](0))
  testBitfield("000", Seq[Byte](0))
  testBitfield("00000000", Seq[Byte](0))
  testBitfield("00000000 0", Seq[Byte](0, 0))
  testBitfield("00000000 00", Seq[Byte](0, 0))
  testBitfield("00000000 00000000", Seq[Byte](0, 0))
  testBitfield("00000000 00000000 0", Seq[Byte](0, 0, 0))

  testBitfield("1", Seq[Byte](1 << 7))
  testBitfield("01", Seq[Byte](1 << 6))
  testBitfield("001", Seq[Byte](1 << 5))
  testBitfield("00000001", Seq[Byte](1))
  testBitfield("00000000 1", Seq[Byte](0, 1 << 7))
  testBitfield("00000000 10", Seq[Byte](0, 1 << 7))
  testBitfield("00000000 100", Seq[Byte](0, 1 << 7))
  testBitfield("00000000 10000000", Seq[Byte](0, 1 << 7))
  testBitfield("00000000 10000000 0", Seq[Byte](0, 1 << 7, 0))
  testBitfield("00000000 01", Seq[Byte](0, 1 << 6))
  testBitfield("00000000 00000001", Seq[Byte](0, 1))
  testBitfield("00000000 00000000 1", Seq[Byte](0, 0, 1 << 7))
  testBitfield("00000000 00000000 10", Seq[Byte](0, 0, 1 << 7))
  testBitfield("00000000 00000000 100", Seq[Byte](0, 0, 1 << 7))
  testBitfield("00000000 00000000 1001", Seq[Byte](0, 0, 1 << 7 | 1 << 4))
}