package krvoje.znjtorrent

import org.joda.time.{DateTime, DateTimeZone}

import scala.io.Source
import org.specs2._

class MetainfoParsingTest extends mutable.Specification {

  "Ubuntu torrent metainfo" >> {
    val result = Metainfo.read(Source.fromResource("ubuntu-12.04.4-server-amd64.iso.torrent").mkString)
    println(result)
    result ==== Metainfo(
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
