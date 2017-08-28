package krvoje.znjtorrent

import java.net.InetSocketAddress

case class PeerwireConnection(address: InetSocketAddress,
  var choked: Boolean,
  var chokesMe: Boolean)

case class Client(
  cons: Seq[PeerwireConnection]
) {


}
