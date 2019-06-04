package krvoje.znjtorrent.torrent

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import akka.io.{IO, Tcp}
import akka.util.ByteString
import krvoje.znjtorrent.peerwire.message._
import krvoje.znjtorrent.peerwire.value.Index

import scala.collection.mutable
import scala.util.{Failure, Success, Try}


/**
  * Handles a connection towards a peer
  */
class Peer(
  remote: InetSocketAddress,
  listener: ActorRef, // TODO: Has to be something that understands general torrent management msgs
  var isChoked: Boolean,
  var isInterested: Boolean,
  var amChoked: Boolean,
  var amInterested: Boolean
) extends Actor {

  import Tcp._
  import context.system

  val log = Logging(context.system, this)

  val manager = IO(Tcp)
  manager ! Connect(remote)

  var handshake: Option[Handshake] = None
  var port: Option[Port] = None
  val hasPiece: mutable.HashMap[Index, Boolean] = mutable.HashMap.empty
  var wantsPieces: Seq[Request] = Seq.empty[Request]

  val ALIVE_TRESHOLD: Long = 2 * 60 * 1000 // Two minutes // TODO: Use duration
  var aliveCounter: Long = System.currentTimeMillis()
  def isAlive: Boolean = (System.currentTimeMillis() - aliveCounter) < ALIVE_TRESHOLD

  override def receive = {
    case CommandFailed(_: Connect) =>
      listener ! "Connection failed"
      context.stop(self)

    case c@Connected(remote, local) =>
      listener ! c
      val connection = sender()
      connection ! Register(self)

      context.become(waitingForHandshake(connection))
  }

  private def waitingForHandshake(connection: ActorRef): Receive = {
    case msg: PeerwireMessage =>
      connection ! Write(ByteString.fromArray(msg.serialized))

    case data: ByteString =>
      connection ! Write(data)

    case CommandFailed(w: Write) =>
      listener ! "write failed (OS buffer full probably)"

    case Received(data) =>

      Try(PeerwireMessage.deserialize(data.asByteBuffer.array())) match {
        case Failure(exception) =>
          log.error(exception, s"Failed deserializing $data")

        case Success(peerwireMessage) =>
          peerwireMessage match {
            case msg@Handshake(_, _) =>  handle(msg, connection)
            case _ => log.warning(s"Expected a handshake, but got a $peerwireMessage. Ignoring")
          }
      }

      listener ! data

    case "close" =>
      connection ! Close

    case _: ConnectionClosed =>
      listener ! "Connection closed"
      context.stop(self)
  }

  private def shookHands(connection: ActorRef): Receive = {
    case msg: PeerwireMessage =>
      connection ! Write(ByteString.fromArray(msg.serialized))

    case data: ByteString =>
      connection ! Write(data)

    case CommandFailed(w: Write) =>
      listener ! "write failed (OS buffer full probably)"

    case Received(data) =>

      Try(PeerwireMessage.deserialize(data.asByteBuffer.array())) match {
        case Failure(exception) =>
          log.error(exception, s"Failed deserializing $data")

        case Success(peerwireMessage) =>
          peerwireMessage match {
            case msg@Handshake(_, _) =>   log.warning(s"Handshake received $msg, but already shook hands.")
            case msg@Keepalive() =>       handle(msg)
            case msg@Choke() =>           handle(msg)
            case msg@Unchoke() =>         handle(msg)
            case msg@Interested() =>      handle(msg)
            case msg@NotInterested() =>   handle(msg)
            case msg@Have(_) =>           handle(msg)
            case msg@Bitfield(_) =>       handle(msg)
            case msg@Request(_, _, _) =>  handle(msg)
            case msg@Piece(_, _, _) =>    handle(msg)
            case msg@Cancel(_, _, _) =>   handle(msg)
            case msg@Port(_) =>           handle(msg)
            case _ => log.warning(s"Unknown PeerwireMessage $peerwireMessage")
          }
      }


      listener ! data

    case "close" =>
      connection ! Close

    case _: ConnectionClosed =>
      listener ! "Connection closed"
      context.stop(self)
  }

  private def handle(msg: Handshake, connection: ActorRef) = {
    this.handshake = Some(msg)
    context.become(shookHands(connection))
  }

  private def handle(msg: Keepalive) = {
    aliveCounter = System.currentTimeMillis()
  }

  private def handle(msg: Choke) = isChoked = true

  private def handle(msg: Unchoke) = isChoked = false

  private def handle(msg: Interested) = isInterested = true

  private def handle(msg: NotInterested) = isInterested = false

  private def handle(msg: Have) = {
    hasPiece(msg.index) = true
  }

  // TODO: Only can be sent as a first message, ignored otherwise
  private def handle(msg: Bitfield): Unit = {
    msg.payload.zipWithIndex.map {
      case (has, index) =>
        hasPiece(index) = has
    }
  }

  private def handle(msg: Request) = {
    wantsPieces ++= Seq(msg)
  }

  private def handle(msg: Piece) = ???

  private def handle(msg: Cancel) = ???

  private def handle(msg: Port) = {
    port = Some(msg)
  }

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