package krvoje.znjtorrent

import java.net.URLEncoder

import krvoje.znjtorrent.bencoding._

sealed trait TrackerEvent {
  val value: String
}
object TrackerEvent {
  case object Started extends TrackerEvent {val value = "started"}
  case object Stopped extends TrackerEvent {val value = "stopped"}
  case object Completed extends TrackerEvent {val value = "completed"}
}

case class TrackerRequest(
  infoHash: String,
  peerID: String,
  port: Int,
  uploaded: BigInt,
  downloaded: BigInt,
  left: BigInt,
  compact: Boolean,
  noPeerID: Boolean,
  event: TrackerEvent,
  ip: String,
  numWant: Int,
  key: String,
  trackerID: String) {

  def requestParams = Seq(
    ("info_hash" -> infoHash),
    ("peer_id" -> peerID),
    ("port" -> port.toString),
    ("uploaded" -> uploaded.toString),
    ("downloaded" -> downloaded.toString),
    ("left" -> left.toString),
    ("compact" -> (if(compact) "1" else "0")),
    ("noPeerID" -> (if(noPeerID) "1" else "0")),
    ("event" -> event.value),
    ("ip" -> ip),
    ("numwant" -> numWant.toString),
    ("key" -> key),
    ("trackerid" -> trackerID))

  def uriString: String = requestParams.map(_._1).mkString("&") // TODO: Escape

  def encode(str: String): String = URLEncoder.encode(str, "utf-8")

}

case class Peer(peerID: String, ip: String, port: Int)
case class TrackerFailure(failureReason: String)
case class TrackerStats(
  warningMessage: Option[String],
  interval: Int,
  minInterval: Option[Int],
  trackerID: String,
  complete: Int,
  incomplete: Int,
  peers: Seq[Peer]
)

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

  private def peers(d: BE): Seq[Peer] = {
    d match {
      case BEList(values @ _*) => values.map(_.asInstanceOf[BEDictionary]).map( d =>
        Peer(
          peerID = d.string("peer id"),
          ip = d.string("ip"),
          port = d.int("port")
        ))
      case BEString(value) => ???
      case _ => throw new InvalidParam("Dictionary or string expected")
    }
  }
}