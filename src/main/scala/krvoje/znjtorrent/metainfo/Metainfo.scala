/*
 * MIT License
 *
 * Copyright (c) 2019 Hrvoje Peradin
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */

package krvoje.znjtorrent.metainfo

import krvoje.znjtorrent.bencoding._
import org.joda.time.{DateTime, DateTimeZone}

case class Metainfo(
                     info        : Info,
                     announce    : String,
                     announceList: Option[Seq[Seq[String]]],
                     creationDate: Option[DateTime],
                     comment     : Option[String],
                     createdBy   : Option[String],
                     encoding    : Option[String]
                   )

case class Info(
                 pieceLength: Int,
                 pieces     : String,
                 isPrivate  : Boolean,
                 name       : String,
                 files      : Seq[File]
               ) {
  val isSingleFileMode  : Boolean = files.isEmpty
  val isMultipleFileMode: Boolean = !isSingleFileMode
}

case class File(
                 path  : String,
                 length: Int,
                 md5sum: Option[String]
               )

object Metainfo {

  def read(content: String): Metainfo = {
    val decoded = BEDeserializer(content).decode.asInstanceOf[BEDictionary]
    require(decoded.isInstanceOf[BEDictionary], "The metainfo file needs to contain a single dictionary")
    read(decoded)
  }

  def read(d: BEDictionary): Metainfo = Metainfo(
    info = info(d.dict("info")),
    announce = d.dict("announce").asInstanceOf[BEString].value,
    announceList = d.dict.get("announce-list").map(_.asInstanceOf[BEList].value.map(_.asInstanceOf[BEList].value.map(_.asInstanceOf[BEString].value))),
    creationDate = d.dict.get("creation date").map(_.asInstanceOf[BEInt].value).map(i => new DateTime(i * 1000L, DateTimeZone.UTC)),
    comment = d.dict.get("comment").map(_.asInstanceOf[BEString].value),
    createdBy = d.dict.get("created by").map(_.asInstanceOf[BEString].value),
    encoding = d.dict.get("encoding").map(_.asInstanceOf[BEString].value)
  )

  private def info(e: BEValue): Info = {
    val d = e.asInstanceOf[BEDictionary]
    Info(
      pieceLength = d.dict("piece length").asInstanceOf[BEInt].value,
      pieces = d.dict("pieces").asInstanceOf[BEString].value,
      isPrivate = d.dict.get("private").exists(_.asInstanceOf[BEInt].value == 1),
      name = d.dict("name").asInstanceOf[BEString].value,
      files = d.dict.get("files")
        .map(fs => fs.asInstanceOf[BEList].value.map(_.asInstanceOf[BEDictionary]))
        .map(fileDicts => fileDicts.map(fileDict => File(
          path = fileDict.dict("path").asInstanceOf[BEList].value.map(_.asInstanceOf[BEString].value).mkString(""),
          length = fileDict.dict("length").asInstanceOf[BEInt].value,
          md5sum = fileDict.dict.get("md5sum").map(_.asInstanceOf[BEString].value)
        )))
        .getOrElse(
          Seq(File(
            path = d.dict("name").asInstanceOf[BEString].value,
            length = d.dict("length").asInstanceOf[BEInt].value,
            md5sum = d.dict.get("md5sum").map(_.asInstanceOf[BEString].value)))))
  }
}
