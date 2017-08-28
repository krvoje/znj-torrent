package krvoje.znjtorrent.tracker

import krvoje.znjtorrent.bencoding._

case class TrackerResponse(
  trackerFailure: Option[TrackerFailure], // Not Either, since both can be present
  stats: Option[TrackerStats]
)

object TrackerResponse {
  def decode(content: String): TrackerResponse = {
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

  private def peers(d: BE): Seq[TrackerPeer] = {
    d match {
      case BEList(values @ _*) => values.map(_.asInstanceOf[BEDictionary]).map( d =>
        TrackerPeer(
          peerID = d.string("peer id"),
          ip = d.string("ip"),
          port = d.int("port")
        ))
      case BEString(value) => ???
      case _ => throw new InvalidParam("Dictionary or string expected")
    }
  }
}