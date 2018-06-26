package krvoje.znjtorrent.tracker

import java.net.InetSocketAddress

import akka.util.ByteString
import krvoje.znjtorrent.bencoding._

case class TrackerResponse(
  trackerFailure: Option[TrackerFailure], // Not Either, since both can be present
  stats: Option[TrackerStats]
)

object TrackerResponse {
  def decode(content: ByteString): TrackerResponse = {
    val decoded = BEDecoder(content).decode
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

  private def peers(d: BE): Seq[TrackerPeer] = { // TODO: Test
    d match {
      case BEList(values @ _*) => values.map(_.asInstanceOf[BEDictionary]).map( d =>
        TrackerPeer(
          peerID = d.string("peer id"),
          address = new InetSocketAddress(d.string("ip"), d.int("port"))
        ))
      case BEString(value) => {
        value.getBytes().grouped(6).flatMap {
          chunk =>
            if(chunk.size == 6) Some {
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