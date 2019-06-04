package krvoje.znjtorrent.torrent

import akka.actor.{Actor, ActorRef}
import krvoje.znjtorrent.metainfo.Metainfo
import krvoje.znjtorrent.torrent.Peer

/**
  * Handles a single torrent
  */
class Torrent(
  metainfo: Metainfo,
  listener: ActorRef
) extends Actor {

  var peers: Seq[Peer] = Seq.empty[Peer]

  override def receive = ???
}
