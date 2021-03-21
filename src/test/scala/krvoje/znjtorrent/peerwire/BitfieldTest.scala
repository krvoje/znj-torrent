package krvoje.znjtorrent.peerwire

import krvoje.znjtorrent.peerwire.message.Bitfield
import org.specs2._
import org.specs2.specification.core.Fragment

class BitfieldTest extends mutable.Specification {

  var count = 0

  implicit def stringToBools(str: String): Array[Boolean] = {
    (for (c <- str.toCharArray) yield {
      if (c == '0') Some(false)
      else if (c == '1') Some(true)
      else None
    }).flatten
  }

  implicit def int2Byte(int: Int) = int.asInstanceOf[Byte]

  implicit def t(f: (String, Seq[Byte])): Fragment = {
    val payload  = f._1
    val expected = f._2.toSeq
    val actual   = Bitfield(payload).value.toSeq
    count += 1
    val title = s"$count. ${payload.grouped(8).mkString(" ")}: $actual vs $expected"
    title >> {
      actual ==== expected
    }
  }

  t("0" -> Seq[Byte](0))
  t("00" -> Seq[Byte](0))
  t("000" -> Seq[Byte](0))
  t("00000000" -> Seq[Byte](0))
  t("00000000 0" -> Seq[Byte](0, 0))
  t("00000000 00" -> Seq[Byte](0, 0))
  t("00000000 00000000" -> Seq[Byte](0, 0))
  t("00000000 00000000 0" -> Seq[Byte](0, 0, 0))

  t("1" -> Seq[Byte](1 << 7))
  t("01" -> Seq[Byte](1 << 6))
  t("001" -> Seq[Byte](1 << 5))
  t("00000001" -> Seq[Byte](1))
  t("00000000 1" -> Seq[Byte](0, 1 << 7))
  t("00000000 10" -> Seq[Byte](0, 1 << 7))
  t("00000000 100" -> Seq[Byte](0, 1 << 7))
  t("00000000 10000000" -> Seq[Byte](0, 1 << 7))
  t("00000000 10000000 0" -> Seq[Byte](0, 1 << 7, 0))
  t("00000000 01" -> Seq[Byte](0, 1 << 6))
  t("00000000 00000001" -> Seq[Byte](0, 1))
  t("00000000 00000000 1" -> Seq[Byte](0, 0, 1 << 7))
  t("00000000 00000000 10" -> Seq[Byte](0, 0, 1 << 7))
  t("00000000 00000000 100" -> Seq[Byte](0, 0, 1 << 7))
  t("00000000 00000000 1001" -> Seq[Byte](0, 0, 1 << 7 | 1 << 4))
}
