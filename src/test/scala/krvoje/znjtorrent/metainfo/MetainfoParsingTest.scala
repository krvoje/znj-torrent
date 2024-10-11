package krvoje.znjtorrent.metainfo

import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class MetainfoParsingTest extends AnyFlatSpec with Matchers {

  "Ubuntu torrent metainfo" should "be parsed correctly" in {
    val result = Metainfo.read(Source.fromResource("ubuntu-12.04.4-server-amd64.iso.torrent").mkString)
    result shouldEqual Metainfo(
      info = Info(pieceLength = 524288,
        pieces = "demo",
        isPrivate = false,
        name = "ubuntu-12.04.4-server-amd64.iso",
        files = Seq(File("ubuntu-12.04.4-server-amd64.iso", 711983104, None))),
      announce = "http://torrent.ubuntu.com:6969/announce",
      announceList = Some(Seq(
        Seq("http://torrent.ubuntu.com:6969/announce"),
        Seq("http://ipv6.torrent.ubuntu.com:6969/announce"))),
      creationDate = Some(new DateTime("2014-02-06T17:12:45.000Z", DateTimeZone.UTC)),
      comment = Some("Ubuntu CD releases.ubuntu.com"),
      createdBy = None,
      encoding = None)
  }

}