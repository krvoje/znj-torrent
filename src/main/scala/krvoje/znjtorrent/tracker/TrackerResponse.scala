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

package krvoje.znjtorrent.tracker

import java.net.InetSocketAddress

import krvoje.znjtorrent.bencoding._

case class TrackerResponse(
  trackerFailure: Option[TrackerFailure], // Not Either, since both can be present
  stats: Option[TrackerStats]
)

//noinspection ScalaStyle
object TrackerResponse {
  def decode(content: String): TrackerResponse = {
    val decoded = BEDeserializer(content).decode
    require(decoded.isInstanceOf[BEDictionary], "The tracker response must be a dictionary")
    val d = decoded.asInstanceOf[BEDictionary]
    val trackerFailure = d.dict.get("failure reason").map(v => TrackerFailure(v.asInstanceOf[BEString].value))

    TrackerResponse(
      trackerFailure = trackerFailure,
      stats = if(trackerFailure.isEmpty) None else Some(
        TrackerStats(
          warningMessage = d.stringOpt("warning message"),
          interval = d.int("interval"),
          minInterval = d.intOpt("min interval"),
          trackerID = d.string("tracker id"),
          complete = d.int("complete"),
          incomplete = d.int("incomplete"),
          peers = peers(d.dict("peers"))
        )
      )
    )
  }

  private def peers(d: BEValue): Seq[TrackerPeer] = { // TODO: Test
    d match {
      case BEList(values @ _*) => values.map(_.asInstanceOf[BEDictionary]).map( d =>
        TrackerPeer(
          peerID = d.string("peer id"),
          address = new InetSocketAddress(d.string("ip"), d.int("port"))
        ))
      case BEString(value) => {
        value.getBytes().grouped(6).flatMap {
          chunk =>
            if(chunk.length == 6) Some {
              val host = Seq(
                chunk(0).asInstanceOf[Int].toString,
                chunk(1).asInstanceOf[Int].toString,
                chunk(2).asInstanceOf[Int].toString,
                chunk(3).asInstanceOf[Int].toString).mkString(".")
              val port: Int = BigInt(chunk.slice(4,6)).intValue()
              TrackerPeer(
                peerID = s"Peer-$host",
                address = new InetSocketAddress(host, port)
              )
            } else None
        }
      }.toSeq
      case _ => throw new InvalidParam("Dictionary or string expected")
    }
  }
}
