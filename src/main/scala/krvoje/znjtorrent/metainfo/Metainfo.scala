package krvoje.znjtorrent.metainfo

import akka.util.ByteString
import krvoje.znjtorrent.bencoding._
import org.joda.time.{DateTime, DateTimeZone}

case class Metainfo(
  info: Info,
  announce: String,
  announceList: Option[Seq[Seq[String]]],
  creationDate: Option[DateTime],
  comment: Option[String],
  createdBy: Option[String],
  encoding: Option[String]
)

case class Info(
  pieceLength: Int,
  pieces: String,
  isPrivate: Boolean,
  name: String,
  files: Seq[File]
) {
  val isSingleFileMode: Boolean = files.length == 0
  val isMultipleFileMode: Boolean = !isSingleFileMode
}

case class File(
  path: String,
  length: Int,
  md5sum: Option[String]
)

object Metainfo {

  def read(content: ByteString): Metainfo = {
    val decoded = BEDecoder(content).decode.asInstanceOf[BEDictionary]
    require(decoded.isInstanceOf[BEDictionary], "The metainfo file needs to contain a single dictionary")
    read(decoded)
  }

  def read(d: BEDictionary): Metainfo = Metainfo(
    info          = info(d.dict("info")),
    announce      = d.dict("announce").asInstanceOf[BEString].value,
    announceList  = d.dict.get("announce-list").map(_.asInstanceOf[BEList].values.map(_.asInstanceOf[BEList].values.map(_.asInstanceOf[BEString].value))),
    creationDate  = d.dict.get("creation date").map(_.asInstanceOf[BEInt].value).map(i => new DateTime(i*1000L, DateTimeZone.UTC)),
    comment       = d.dict.get("comment").map(_.asInstanceOf[BEString].value),
    createdBy     = d.dict.get("created by").map(_.asInstanceOf[BEString].value),
    encoding      = d.dict.get("encoding").map(_.asInstanceOf[BEString].value)
  )

  private def info(e: BE): Info = {
    val d = e.asInstanceOf[BEDictionary]
    Info(
      pieceLength = d.dict("piece length").asInstanceOf[BEInt].value,
      pieces      = d.dict("pieces").asInstanceOf[BEString].value,
      isPrivate   = d.dict.get("private").exists(_.asInstanceOf[BEInt].value == 1),
      name        = d.dict("name").asInstanceOf[BEString].value,
      files       = d.dict.get("files")
        .map( fs => fs.asInstanceOf[BEList].values.map(_.asInstanceOf[BEDictionary]))
        .map( fileDicts => fileDicts.map( fileDict=> File(
          path    = fileDict.dict("path").asInstanceOf[BEList].values.map(_.asInstanceOf[BEString].value).mkString(""),
          length  = fileDict.dict("length").asInstanceOf[BEInt].value,
          md5sum  = fileDict.dict.get("md5sum").map(_.asInstanceOf[BEString].value)
        )))
        .getOrElse(
          Seq(File(
            path    = d.dict("name").asInstanceOf[BEString].value,
            length  = d.dict("length").asInstanceOf[BEInt].value,
            md5sum  = d.dict.get("md5sum").map(_.asInstanceOf[BEString].value)))))
  }
}