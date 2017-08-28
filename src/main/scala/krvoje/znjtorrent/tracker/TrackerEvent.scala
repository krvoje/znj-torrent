package krvoje.znjtorrent.tracker

sealed trait TrackerEvent {
  val value: String
}

object TrackerEvent {
  case object Started extends TrackerEvent {val value = "started"}
  case object Stopped extends TrackerEvent {val value = "stopped"}
  case object Completed extends TrackerEvent {val value = "completed"}
}