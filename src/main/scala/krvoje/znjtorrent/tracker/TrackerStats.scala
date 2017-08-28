package krvoje.znjtorrent.tracker

case class TrackerStats(
  warningMessage: Option[String],
  interval: Int,
  minInterval: Option[Int],
  trackerID: String,
  complete: Int,
  incomplete: Int,
  peers: Seq[TrackerPeer]
)
