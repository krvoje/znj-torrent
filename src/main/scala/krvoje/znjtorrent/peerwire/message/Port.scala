package krvoje.znjtorrent.peerwire.message

import krvoje.znjtorrent.peerwire.value.PortValue

case class Port(port: PortValue) extends PeerwireMessage {
  override val len: Int = 3
  override val ser: Array[Byte] =  Array.concat(
    LEN,
    Array(Port.ID),
    port.ser)
}

object Port extends Deser[Port] {
  override val ID: Byte = 9
  override def deser(bs: Array[Byte]) = {
    Port(PortValue(bs.slice(5,7)))
  }

  def apply(portValue: Int): Port = Port(PortValue(portValue))
}