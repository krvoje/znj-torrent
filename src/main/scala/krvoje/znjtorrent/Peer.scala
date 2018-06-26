package krvoje.znjtorrent

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.io.{IO, Tcp}
import akka.util.ByteString
import krvoje.znjtorrent.peerwire.message._

import scala.util.{Failure, Success, Try}


class Peer(
  remote: InetSocketAddress,
  var choked: Boolean,
  var interested: Boolean,
  var chokesMe: Boolean,
  var interestsMe: Boolean,
  listener: ActorRef) extends Actor {

  import Tcp._
  import context.system

  val manager = IO(Tcp)
  manager ! Connect(remote)

  override def receive = {
    case CommandFailed(_: Connect) =>
      listener ! "Connection failed"
      context.stop(self)

    case c@Connected(remote, local) =>
      listener ! c
      val connection = sender()
      connection ! Register(self)
      context.become {
        case data: ByteString =>
          connection ! Write(data)

        case CommandFailed(w: Write) =>
          listener ! "write failed (OS buffer full probably)"

        case Received(data) =>

          Try(PeerwireMessage.deserialize(data.asByteBuffer.array())) match {
            case Failure(exception) =>
            // TODO: Handle failure

            case Success(peerwireMessage) =>
              peerwireMessage match {
                case msg@Handshake(infoHash, peerID) =>   handle(msg)
                case msg@Keepalive() =>                   handle(msg)
                case msg@Choke() =>                       handle(msg)
                case msg@Unchoke() =>                     handle(msg)
                case msg@Interested() =>                  handle(msg)
                case msg@NotInterested() =>               handle(msg)
                case msg@Have(index) =>                   handle(msg)
                case msg@Bitfield(payload) =>             handle(msg)
                case msg@Request(index, begin, end) =>    handle(msg)
                case msg@Piece(index, begin, block) =>    handle(msg)
                case msg@Cancel(index, begin, end) =>     handle(msg)
                case msg@Port(port) =>                    handle(msg)
                case _ => // TODO: Handle
              }
          }


          listener ! data

        case "close" =>
          connection ! Close

        case _: ConnectionClosed =>
          listener ! "Connection closed"
          context.stop(self)
      }
  }

  def handle(msg: Handshake) = ???
  def handle(msg: Keepalive) = ???
  def handle(msg: Choke) = ???
  def handle(msg: Unchoke) = ???
  def handle(msg: Interested) = ???
  def handle(msg: NotInterested) = ???
  def handle(msg: Have) = ???
  def handle(msg: Bitfield) = ???
  def handle(msg: Request) = ???
  def handle(msg: Piece) = ???
  def handle(msg: Cancel) = ???
  def handle(msg: Port) = ???

}

object Peer {
  def props(remote: InetSocketAddress, replies: ActorRef) =
    Props(classOf[Peer],
      remote,
      true,
      false,
      true,
      false,
      replies)
}