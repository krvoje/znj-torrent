package krvoje.znjtorrent

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.io.Tcp

class Peer(address: InetSocketAddress,
  var choked: Boolean,
  var interested: Boolean,
  var chokesMe: Boolean,
  var interestsMe: Boolean,
  listener: ActorRef) extends Actor {

  import Tcp._

  override def receive = {
    case CommandFailed(_: Connect) =>
      listener ! "Connection failed" // TODO: See wtf is listener
      context.stop(self)
    case c@Connected(remote, local) =>
      listener ! c
  }

}

object Peer {
  def props(address: InetSocketAddress,
    replies: ActorRef) =
    Props(classOf[Peer],
        address,
      true, false,
      true, false,
      replies)
}