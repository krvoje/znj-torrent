package krvoje.znjtorrent.tracker

import java.net.URLEncoder

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







