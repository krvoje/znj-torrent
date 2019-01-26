package krvoje.znjtorrent.bencoding


// TODO: Fix these
class InvalidParam(message: String) extends Exception
class InvalidBEInt(message: String) extends InvalidParam(message)
class InvalidStringPrefix(message: String) extends InvalidParam(message)
class InvalidList(message: String, bEList: BEList) extends InvalidParam(message)